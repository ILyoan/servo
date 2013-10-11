/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

/// Some little helpers for hooking up the HTML parser with the CSS parser.

use std::cell::Cell;
use std::comm;
use std::comm::Port;
use std::task;
use std::str;
use newcss::stylesheet::Stylesheet;
use newcss::util::DataStream;
use servo_net::resource_task::{ResourceTask, ProgressMsg, Load, Payload, Done, UrlChange};
use extra::url::Url;

/// Where a style sheet comes from.
pub enum StylesheetProvenance {
    UrlProvenance(Url),
    InlineProvenance(Url, ~str),
}

pub struct CSSData {
    sheet: Stylesheet,
    data: Option<~str>
}

pub fn spawn_css_parser(provenance: StylesheetProvenance,
                        resource_task: ResourceTask)
                     -> Port<CSSData> {
    let (result_port, result_chan) = comm::stream();

    let provenance_cell = Cell::new(provenance);
    do task::spawn {
        let url = do provenance_cell.with_ref |p| {
            match *p {
                UrlProvenance(ref the_url) => (*the_url).clone(),
                InlineProvenance(ref the_url, _) => (*the_url).clone()
            }
        };

        let sheet;
        let data_stream = data_stream(provenance_cell.take(), resource_task.clone());
        match data_stream {
            Some(ref data) => { 
                sheet = Stylesheet::new(url, data_to_datastream(data.to_owned()));
            }
            None => { sheet = Stylesheet::new(url, || None); }
        }

        let cssdata = CSSData { sheet: sheet, data: data_stream };

        result_chan.send(cssdata);
    }

    return result_port;
}

fn data_stream(provenance: StylesheetProvenance, resource_task: ResourceTask) -> Option<~str> {
    match provenance {
        UrlProvenance(url) => {
            let (input_port, input_chan) = comm::stream();
            resource_task.send(Load(url, input_chan));
            resource_port_to_data_stream(input_port)
        }
        InlineProvenance(_, data) => {
            data_to_option(data.to_owned())
        }
    }
}

fn resource_port_to_data_stream(input_port: Port<ProgressMsg>) -> Option<~str> {
    let mut result = None;
    loop {
        match input_port.recv() {
            UrlChange(*) => (),  // don't care that URL changed
                Payload(data) => {
                    unsafe {
                        result = Some(str::raw::from_utf8(data));
                    }
                    break;
                }
            Done(*) => break
        }
    }
    result
}

fn data_to_option(data: ~str) -> Option<~str> {
    if data.is_empty() { None }
    else { Some(data.to_owned()) }
}

fn data_to_datastream(data: ~str) -> DataStream {
    let data_cell = Cell::new(data);
    return || {
        if data_cell.is_empty() {
            None
        } else {
            // FIXME: Blech, a copy.
            let data = data_cell.take();
            Some(data.as_bytes().to_owned())
        }
    }
}
