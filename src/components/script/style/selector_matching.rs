/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::ascii::StrAsciiExt;
use extra::sort::tim_sort;

use style::selectors::*;
use style::stylesheets::parse_stylesheet;
use style::media_queries::{Device, Screen};
use style::properties::{ComputedValues, cascade, PropertyDeclaration, get_initial_values};
use dom::node::{AbstractNode, LayoutView};
use dom::element::Element;

use std::cell::Cell;
use std::cast;
use std::comm;
use std::vec;
use std::task;
use wapcaplet::LwcString;
use lwcstr_from_rust_str = wapcaplet::from_rust_string;
use servo_util::tree::TreeNodeRef;
use extra::time::precise_time_ns;

pub enum StylesheetOrigin {
    UserAgentOrigin,
    AuthorOrigin,
    UserOrigin,
}


pub struct Stylist {
    priv ua_rules: PerOriginRules,
    priv author_rules: PerOriginRules,
    priv user_rules: PerOriginRules,
}


impl Stylist {
    #[inline]
    pub fn new() -> Stylist {
        Stylist {
            ua_rules: PerOriginRules::new(),
            author_rules: PerOriginRules::new(),
            user_rules: PerOriginRules::new(),
        }
    }

    pub fn add_stylesheet(&mut self, css_source: &str, origin: StylesheetOrigin) {
        let stylesheet = parse_stylesheet(css_source);
        let rules = match origin {
            UserAgentOrigin => &mut self.ua_rules,
            AuthorOrigin => &mut self.author_rules,
            UserOrigin => &mut self.user_rules,
        };
        let mut added_normal_declarations = false;
        let mut added_important_declarations = false;

        macro_rules! append(
            ($priority: ident, $flag: ident) => {
                if style_rule.declarations.$priority.len() > 0 {
                    $flag = true;
                    for selector in style_rule.selectors.iter() {
                        rules.$priority.push(Rule {
                            selector: *selector,
                            declarations: style_rule.declarations.$priority,
                        })
                    }
                }
            };
        )

        let device = &Device { media_type: Screen };  // TODO, use Print when printing
        for style_rule in stylesheet.iter_style_rules(device) {
            append!(normal, added_normal_declarations);
            append!(important, added_important_declarations);
        }

        // These sorts need to be stable
        // Do not sort already-sorted unchanged vectors
        if added_normal_declarations {
            tim_sort(rules.normal)
        }
        if added_important_declarations {
            tim_sort(rules.important)
        }
    }

    pub fn match_style(&self, element: AbstractNode<LayoutView>) {
        assert!(element.is_element())        
        let mut applicable_declarations = ~[];  // TODO: use an iterator?

        macro_rules! append(
            ($rules: expr) => {
                for rule in $rules.iter() {
                    if matches_selector(rule.selector, element, None) {
                        for declaration in rule.declarations.iter() {
                            applicable_declarations.push((*declaration).clone())
                        }
                    }
                }
            };
        );

        // In cascading order
        append!(self.ua_rules.normal);
        append!(self.user_rules.normal);
        append!(self.author_rules.normal);
        // TODO add style attribute
        append!(self.author_rules.important);
        append!(self.user_rules.important);
        append!(self.ua_rules.important);

        let cell = Cell::new(applicable_declarations);
        do element.write_layout_data |data| {
            data.applicable_declarations = cell.take();
        }
    }
}


struct PerOriginRules {
    normal: ~[Rule],
    important: ~[Rule],
}

impl PerOriginRules {
    #[inline]
    fn new() -> PerOriginRules {
        PerOriginRules { normal: ~[], important: ~[] }
    }
}

#[deriving(Clone)]
struct Rule {
    selector: @Selector,
    declarations: @[PropertyDeclaration],
}


impl Ord for Rule {
    #[inline]
    fn lt(&self, other: &Rule) -> bool {
        self.selector.specificity < other.selector.specificity
    }
}


#[inline]
fn matches_selector(selector: &Selector, element: AbstractNode<LayoutView>,
                    pseudo_element: Option<PseudoElement>) -> bool {
    selector.pseudo_element == pseudo_element &&
    matches_compound_selector(&selector.compound_selectors, element)
}


fn matches_compound_selector(selector: &CompoundSelector,
                             element: AbstractNode<LayoutView>) -> bool {
    if do element.with_imm_element |element| {
        !do selector.simple_selectors.iter().all |simple_selector| {
            matches_simple_selector(simple_selector, element)
        }
    } {
        return false
    }
    match selector.next {
        None => true,
        Some((ref next_selector, combinator)) => {
            let (siblings, just_one) = match combinator {
                Child => (false, true),
                Descendant => (false, false),
                NextSibling => (true, true),
                LaterSibling => (true, false),
            };
            let mut node = element;
            loop {
                match if siblings { node.prev_sibling() } else { node.parent_node() } {
                    None => return false,
                    Some(next_node) => node = next_node,
                }
                if node.is_element() {
                    if matches_compound_selector(&**next_selector, node) {
                        return true
                    } else if just_one {
                        return false
                    }
                }
            }
        }
    }
}

#[inline]
fn matches_simple_selector(selector: &SimpleSelector, element: &Element) -> bool {
    static WHITESPACE: &'static [char] = &'static [' ', '\t', '\n', '\r', '\x0C'];

    match *selector {
        // TODO: case-sensitivity depends on the document type
        LocalNameSelector(ref name) => element.tag_name.eq_ignore_ascii_case(name.as_slice()),
        NamespaceSelector(_) => false,  // TODO, when the DOM supports namespaces on elements.
        // TODO: case-sensitivity depends on the document type and quirks mode
        // TODO: cache on elements.
        IDSelector(ref id) => {
            match element.interned_id {
                None => false,
                Some(ref existing_id) => id == existing_id
            }
        },
        // TODO: cache classe names on elements.
        ClassSelector(ref class) => {
            let mut ret = false;
            for atom in element.interned_classes.iter() {
                if atom == class {
                    ret = true;
                    break;
                }
            }
            ret
        },

        AttrExists(ref attr) => match_attribute(attr, element, |_| true),
        AttrEqual(ref attr, ref value) => match_attribute(attr, element, |v| v == value.as_slice()),
        AttrIncludes(ref attr, ref value) => do match_attribute(attr, element) |attr_value| {
            attr_value.split_iter(WHITESPACE).any(|v| v == value.as_slice())
        },
        AttrDashMatch(ref attr, ref value, ref dashing_value)
        => do match_attribute(attr, element) |attr_value| {
            attr_value == value.as_slice() || attr_value.starts_with(dashing_value.as_slice())
        },
        AttrPrefixMatch(ref attr, ref value) => do match_attribute(attr, element) |attr_value| {
            attr_value.starts_with(value.as_slice())
        },
        AttrSubstringMatch(ref attr, ref value) => do match_attribute(attr, element) |attr_value| {
            attr_value.contains(value.as_slice())
        },
        AttrSuffixMatch(ref attr, ref value) => do match_attribute(attr, element) |attr_value| {
            attr_value.ends_with(value.as_slice())
        },

        Negation(ref negated) => {
            !negated.iter().all(|s| matches_simple_selector(s, element))
        },
    }
}


#[inline]
fn match_attribute(attr: &AttrSelector, element: &Element, f: &fn(&str)-> bool) -> bool {
    match attr.namespace {
        Some(_) => false,  // TODO, when the DOM supports namespaces on attributes
        None => match element.get_attr(attr.name) {
            None => false,
            Some(ref value) => f(value.as_slice())
        }
    }
}


pub trait StyleMethod {
    fn match_subtree(&self, stylist: &Stylist);
    fn cascade_subtree(&self, parent: Option<AbstractNode<LayoutView>>);

    fn match_subtree_task(&self, stylist: &Stylist);
    fn cascade_subtree_task(&self, parent: Option<AbstractNode<LayoutView>>);
}

impl StyleMethod for AbstractNode<LayoutView> {
    fn match_subtree(&self, stylist: &Stylist) {
        stylist.match_style(*self);

        for kid in self.children() {
            if kid.is_element() {
                kid.match_subtree(stylist);
            }
        }
    }

    fn cascade_subtree(&self, parent: Option<AbstractNode<LayoutView>>) {
        let computed_values = do self.read_layout_data |data| {
            match parent {
                Some(p) => p.read_layout_data(|pdata| cascade(data.applicable_declarations, pdata.style_sapin.map(|v| v))),
                None => cascade(data.applicable_declarations, None)
            }
        };

        let cell = Cell::new(computed_values);
        self.write_layout_data(|data| data.style_sapin = Some(cell.take()));

        for kid in self.children() {
            if kid.is_element() {
                kid.cascade_subtree(Some(*self));
            }
        }
    }

    fn match_subtree_task(&self, stylist: &Stylist) {
        let num_task = 8;
        let mut count = 0;
        let mut node_per_task = vec::from_elem(num_task, ~[]);
        for node in self.traverse_preorder() {
            if node.is_element() {
                node_per_task[count % num_task].push(node);
                count += 1;
            }
        }

        let (port, chan) = comm::stream();
        let chan = comm::SharedChan::new(chan);
        let mut num_spawn = 0;
        let this = *self;
        let stylist: *Stylist = unsafe { cast::transmute(stylist) };

        let s = precise_time_ns();
        for nodes in node_per_task.move_iter() {
            if nodes.len() > 0 {
                let chan = chan.clone();
                let mut task = task::task();
                task.sched_mode(task::SingleThreaded);
                do task.spawn_with(nodes) |nodes| {
                    let mut count = 0;
                    let stylist: &Stylist = unsafe { cast::transmute(stylist) };
                    for node in nodes.move_iter() {
                        stylist.match_style(node);
                        count += 1;
                    }
                    chan.send(count);
                }
                num_spawn += 1;
            }
        }
        for i in range(0, num_spawn) {
            let count = port.recv();
            printfln!("A match task finished(%?/%?): %? nodes takes %?ms",
                i + 1, num_spawn, count, (precise_time_ns() - s) as float / 1000000f);
        }
    }

    fn cascade_subtree_task(&self, parent: Option<AbstractNode<LayoutView>>) {
        let computed_values = do self.read_layout_data |data| {
            match parent {
                Some(p) => p.read_layout_data(|pdata| cascade(data.applicable_declarations, pdata.style_sapin.map(|v| v))),
                None => cascade(data.applicable_declarations, None)
            }
        };

        let cell = Cell::new(computed_values);
        self.write_layout_data(|data| data.style_sapin = Some(cell.take()));

        let num_task = 2;
        let mut count = 0;
        let mut kids_per_task = vec::from_elem(num_task, ~[]);
        for kid in self.children() {
            if kid.is_element() {
                kids_per_task[count % num_task].push(kid);
                count += 1;
            }
        }

        let (port, chan) = comm::stream();
        let chan = comm::SharedChan::new(chan);

        let mut num_spawn = 0;
        let this = *self;
        for kids in kids_per_task.move_iter() {
            if kids.len() > 0 {
                let chan = chan.clone();
                do task::spawn_with(kids) |kids| {
                    for kid in kids.move_iter() {
                        kid.cascade_subtree_task(Some(this));
                    }
                    chan.send(());
                }
                num_spawn += 1;
            }
        }
        
        for _ in range(0, num_spawn) {
            port.recv();
        }
    }
}