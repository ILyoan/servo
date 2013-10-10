/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

/// Some little helpers for hooking up the HTML parser with the CSS parser.

use std::cell::Cell;
use std::comm;
use std::comm::Port;
use std::task;
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
//                     -> Port<Stylesheet> {
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

        let (data_stream, cssdata) = data_stream(provenance_cell.take(), resource_task.clone());
//        let sheet = Stylesheet::new(url, data_stream, cssdata);
        let sheet = Stylesheet::new(url, data_stream);
        let cssdata = CSSData {
            sheet: sheet,
            data: cssdata
        };
        result_chan.send(cssdata);
    }

    return result_port;
}

fn data_stream(provenance: StylesheetProvenance, resource_task: ResourceTask) -> (DataStream, Option<~str>) {
    match provenance {
        UrlProvenance(url) => {
            error!("cssparse: UrlProvenance(loading stylesheet): %s\n", url.to_str());
            let (input_port, input_chan) = comm::stream();
            resource_task.send(Load(url, input_chan));
            (resource_port_to_data_stream(input_port), None)
        }
        InlineProvenance(_, data) => {
            error!("cssparse: InlineProvenance(loading stylesheet): %s\n", data.clone());
            (data_to_data_stream(data.to_owned()), Some(data.to_owned()))
        }
    }
}

fn resource_port_to_data_stream(input_port: Port<ProgressMsg>) -> DataStream {
    return || {
        // Can't just 'return' the value since we're inside a lambda
        let mut result = None;
        loop {
            match input_port.recv() {
                UrlChange(*) => (),  // don't care that URL changed
                Payload(data) => {
                    result = Some(data);
                    break;
                }
                Done(*) => break
            }
        }
        result
    }
}

fn data_to_data_stream(data: ~str) -> DataStream {
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

