use lazy_static::lazy_static;
use regex::Regex;
use std::fmt::Display;
use thiserror::Error;

#[derive(PartialEq, Debug, Clone)]
pub enum Attribute<'a> {
    Tag(&'a str),
    Pair { key: &'a str, value: &'a str },
}
impl<'a> Display for Attribute<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Attribute::Tag(tag) => write!(f, "{}", tag),
            Attribute::Pair { key, value } => write!(f, "{}={}", key, value),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Flow<'a>(Vec<&'a str>);
impl<'a> From<&'a str> for Flow<'a> {
    fn from(s: &'a str) -> Self {
        Flow(vec![s])
    }
}
impl<'a> Flow<'a> {
    pub(crate) fn append(&mut self, flow: &mut Flow<'a>) {
        self.0.append(&mut flow.0)
    }
}
impl<'a> Display for Flow<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut flow = String::new();
        for s in &self.0 {
            flow.push_str(s);
        }
        write!(f, "{}", flow)
    }
}

#[derive(PartialEq, Debug)]
pub struct Element<'a> {
    name: &'a str,
    attributes: Option<Vec<Attribute<'a>>>,
    flow: Option<Flow<'a>>,
    children: Vec<Child<'a>>,
}
#[derive(PartialEq, Debug)]
pub enum Child<'a> {
    Element(Element<'a>),
    CharacterData(&'a str),
}
impl<'a> Child<'a> {
    pub(crate) fn to_xml(&self, indent: usize) -> String {
        match self {
            Child::Element(e) => e.to_xml(indent),
            Child::CharacterData(s) => String::from(s.clone()),
        }
    }
}
impl<'a> Display for Element<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = format!("{}", self.name);
        let attributes = match &self.attributes {
            Some(attrs) => attrs
                .iter()
                .map(|a| format!("{}", a))
                .collect::<Vec<_>>()
                .join(" "),
            None => String::from(""),
        };
        write!(f, "<{}{}>", name, attributes)
    }
}
impl<'a> Element<'a> {
    pub(crate) fn new(
        name: &'a str,
        attributes: Option<Vec<Attribute<'a>>>,
        flow: Option<Flow<'a>>,
    ) -> Element<'a> {
        Element {
            name,
            attributes,
            flow,
            children: Vec::new(),
        }
    }
    // pub(crate) fn append_character_data(&mut self, cdata: &'a str) {
    //     self.children.push(Child::CharacterData(cdata))
    // }
    pub(crate) fn add_child_element(&mut self, child: Element<'a>) {
        self.children.push(Child::Element(child))
    }
    pub(crate) fn to_xml(&self, indent: usize) -> String {
        let name = format!("{}", self.name);
        let attributes = match &self.attributes {
            Some(attrs) => attrs
                .iter()
                .map(|a| format!(" {}", a))
                .collect::<Vec<_>>()
                .join(""),
            None => String::from(""),
        };
        if self.children.is_empty() {
            format!(
                "{tabs}{element}",
                element = format!("<{}{}/>", name, attributes),
                tabs = "  ".repeat(indent),
            )
        } else {
            let open = format!("<{}{}>", name, attributes);
            let inner = self
                .children
                .iter()
                .map(|c| c.to_xml(indent + 1))
                .collect::<Vec<_>>()
                .join("\n");
            let close = format!("</{}>", name);
            format!(
                "{tabs}{}\n{}\n{tabs}{}",
                open,
                inner,
                close,
                tabs = "  ".repeat(indent),
            )
        }
    }
}

pub struct Tree<'source> {
    pub root: Element<'source>,
}
impl<'source> Tree<'source> {
    pub fn parse(source: &'source str) -> Result<Tree<'source>, ParserError> {
        let mut parse_root = parse(&source)?;
        let root = match parse_root.children.is_empty() {
            false => match parse_root.children.remove(0) {
                Child::Element(root) => root,
                Child::CharacterData(_) => return Err(ParserError::InvalidRootCharacterData),
            },
            true => return Err(ParserError::EmptyTree),
        };

        Ok(Tree { root })
    }
    pub fn to_xml(&self) -> String {
        self.root.to_xml(0)
    }
}

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("No lines to read")]
    EmptyFile,

    #[error("Parser found no elements")]
    EmptyTree,

    #[error("Root element is CharacterData; must be Element")]
    InvalidRootCharacterData,

    #[error("First line must be a block definition")]
    IllegalFirstLineNotBlock,

    #[error("First line must be unindented")]
    IllegalFirstLineIndented,

    #[error("Line {line}: Invalid line break")]
    IllegalLineBreak { line: usize },

    #[error("Line {line}: Invalid paragraph terminator; paragraph must be ended with blank line")]
    IllegalParagraphTerminator { line: usize },

    #[error("Line {line}: Invalid child nested; paragraphs and list items cannot have children")]
    IllegalChild { line: usize },

    #[error("Line {line}: Tried to indent {indents} levels at once; maximum one at a time")]
    MoreThanOneIndent { line: usize, indents: i32 },
}

struct Stack<'a> {
    stack: Vec<Element<'a>>,
}
impl<'a> Stack<'a> {
    pub(crate) fn new(root: Element<'a>) -> Stack<'a> {
        Stack { stack: vec![root] }
    }
    pub(crate) fn get_indent_delta(&self, indent: u16) -> i32 {
        (indent as i32) - (self.stack.len() as i32 - 2)
    }
    pub(crate) fn open(&mut self, element: Element<'a>) {
        self.stack.push(element);
    }
    pub(crate) fn close(&mut self) {
        let e = match self.stack.pop() {
            Some(e) => e,
            None => todo!(),
        };
        let last = match self.stack.last_mut() {
            Some(l) => l,
            None => todo!(),
        };
        last.add_child_element(e);
    }
    pub(crate) fn finalize(&mut self) -> Element<'a> {
        while self.stack.len() > 1 {
            self.close();
        }
        match self.stack.pop() {
            Some(root) => root,
            None => todo!(),
        }
    }
}

fn parse<'a>(source: &'a str) -> Result<Element<'a>, ParserError> {
    let mut lines = source.lines().peekable();
    let root = Element::new("@parse-root", None, None);
    let mut stack = Stack::new(root);
    let mut line_num = 0;
    while let Some(line) = lines.next() {
        line_num += 1;
        let line = parse_line(line);
        let indents = line.get_indents();
        let element = match line {
            Line::Block {
                name,
                attributes,
                flow,
                ..
            } => Element::new(name, attributes, flow),

            Line::OrderedListItem {
                indent: base_indent,
                flow,
            } => {
                let mut ol = Element::new("ol", None, None);
                ol.add_child_element(Element::new("li", None, Some(flow)));
                while let Some(peek) = lines.peek() {
                    match parse_line(peek) {
                        Line::OrderedListItem { indent, flow } => {
                            lines.next(); // consume the peek
                            line_num += 1;

                            if indent != base_indent {
                                return Err(ParserError::IllegalChild { line: line_num });
                            }
                            ol.add_child_element(Element::new("li", None, Some(flow)));
                        }
                        _ => {
                            break;
                        }
                    };
                }
                ol
            }

            Line::UnorderedListItem {
                indent: base_indent,
                flow,
            } => {
                let mut ul = Element::new("ul", None, None);
                ul.add_child_element(Element::new("li", None, Some(flow)));
                while let Some(peek) = lines.peek() {
                    match parse_line(peek) {
                        Line::UnorderedListItem { indent, flow } => {
                            lines.next(); // consume the peek
                            line_num += 1;

                            if indent != base_indent {
                                return Err(ParserError::IllegalChild { line: line_num });
                            }
                            ul.add_child_element(Element::new("li", None, Some(flow)));
                        }
                        _ => {
                            break;
                        }
                    };
                }
                ul
            }

            Line::Paragraph {
                indent: base_indent,
                flow: mut base_flow,
            } => {
                while let Some(peek) = lines.peek() {
                    match parse_line(peek) {
                        Line::Paragraph { indent, mut flow } => {
                            lines.next(); // consume the peek
                            line_num += 1;

                            if indent != base_indent {
                                return Err(ParserError::IllegalChild { line: line_num });
                            }
                            base_flow.append(&mut flow);
                        }
                        Line::Blank => {
                            break;
                        }

                        _ => {
                            return Err(ParserError::IllegalParagraphTerminator {
                                line: line_num + 1,
                            })
                        }
                    };
                }
                Element::new("p", None, Some(base_flow))
            }
            _ => continue,
        };

        // use line indent to update stack
        if let Some(indent) = indents {
            match stack.get_indent_delta(indent) {
                delta if delta > 1 => {
                    return Err(ParserError::MoreThanOneIndent {
                        line: line_num,
                        indents: delta,
                    })
                }
                delta if delta == 1 => stack.open(element),
                delta if delta <= 0 => {
                    while stack.get_indent_delta(indent) <= 0 {
                        stack.close();
                    }
                    stack.open(element)
                }
                _ => {}
            }
        }
    }

    Ok(stack.finalize())
}

#[derive(PartialEq, Debug)]
enum Line<'a> {
    Block {
        indent: u16,
        name: &'a str,
        attributes: Option<Vec<Attribute<'a>>>,
        flow: Option<Flow<'a>>,
    },
    OrderedListItem {
        indent: u16,
        flow: Flow<'a>,
    },
    UnorderedListItem {
        indent: u16,
        flow: Flow<'a>,
    },
    Paragraph {
        indent: u16,
        flow: Flow<'a>,
    },
    Blank,
}
impl<'a> Line<'a> {
    pub(crate) fn get_indents(&self) -> Option<u16> {
        match self {
            Line::Block { indent, .. }
            | Line::OrderedListItem { indent, .. }
            | Line::UnorderedListItem { indent, .. }
            | Line::Paragraph { indent, .. } => Some(*indent),
            Line::Blank => None,
        }
    }
}
fn parse_line<'a>(line: &'a str) -> Line<'a> {
    if line.trim().is_empty() {
        return Line::Blank;
    }

    let (indent, text) = parse_indentations(line);

    if let Some(block_line) = parse_block(indent, text) {
        return block_line;
    }
    if let Some(ordered_list_line) = parse_ordered_list_item(indent, text) {
        return ordered_list_line;
    }
    if let Some(unordered_list_line) = parse_unordered_list_item(indent, text) {
        return unordered_list_line;
    }

    // else its a paragraph
    Line::Paragraph {
        indent,
        flow: Flow::from(text),
    }
}

fn parse_block<'a>(indent: u16, text: &'a str) -> Option<Line> {
    // check for and split on required colon
    let (name_candidate, remainder) = match text.split_once(":") {
        Some((n, r)) => (n, r),
        None => return None, // abort if no colon
    };

    if name_candidate.ends_with("\\") {
        // this colon is escaped
        return None;
    }

    // validate block name
    let name = match is_valid_name_token(name_candidate) {
        true => name_candidate,
        false => return None, // abort if invalid name
    };

    // split attributes and flow
    let (attributes_string, flow) = match remainder.strip_prefix("(") {
        Some(attributes_candidate) => match attributes_candidate.split_once(")") {
            Some((attributes, flow)) => (Some(attributes), flow),
            None => (None, remainder),
        },
        None => (None, remainder),
    };

    // parse attributes string
    let attributes = match attributes_string {
        Some(attributes_string) => parse_attributes(attributes_string),
        None => None,
    };

    // parse flow string
    let flow = match flow.is_empty() {
        false => match flow.strip_prefix(" ") {
            Some(flow) => Some(Flow::from(flow)),
            None => None,
        },
        true => None,
    };

    return Some(Line::Block {
        indent,
        name,
        attributes,
        flow,
    });
}

fn parse_ordered_list_item(indent: u16, text: &str) -> Option<Line> {
    // look for required '.'
    let (number_candidate, flow) = match text.split_once(".") {
        Some((n, r)) => (n, r),
        None => return None,
    };

    // make sure number is valid
    if !number_candidate.parse::<u32>().is_ok() {
        return None;
    }

    // make sure flow has a space after the dot
    let flow = match flow.strip_prefix(" ") {
        Some(flow) => flow,
        None => return None,
    };

    // make sure flow isn't empty
    if flow.is_empty() {
        return None;
    }

    let flow = Flow::from(flow);

    return Some(Line::OrderedListItem { indent, flow });
}

fn parse_unordered_list_item(indent: u16, text: &str) -> Option<Line> {
    // split on first space
    let (asterisk_candidate, flow) = match text.split_once(" ") {
        Some((a, f)) => (a, f),
        None => return None,
    };

    // make sure required '*'
    if !asterisk_candidate.eq("*") {
        return None;
    }

    // make sure flow isn't empty
    if flow.is_empty() {
        return None;
    }

    let flow = Flow::from(flow);

    return Some(Line::UnorderedListItem { indent, flow });
}

fn parse_attributes(attributes_string: &str) -> Option<Vec<Attribute>> {
    let mut attributes = Vec::new();
    for a in attributes_string.split(",").collect::<Vec<_>>() {
        let attribute = match a.split_once("=") {
            Some((key, value)) => Attribute::Pair { key, value },
            None => Attribute::Tag(a),
        };
        attributes.push(attribute);
    }

    match attributes.is_empty() {
        false => Some(attributes),
        true => None,
    }
}

fn parse_indentations(line: &str) -> (u16, &str) {
    let mut line = line;
    let mut indents = 0;
    while let Some(substring) = line.strip_prefix("\t") {
        line = substring;
        indents += 1;
    }
    (indents, line)
}

fn is_valid_name_token(token: &str) -> bool {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"\b[a-z]+(?:['-]?[a-z]+)*\b").expect("Invalid Regex");
    }
    RE.is_match(token)
}

#[cfg(test)]
mod test {
    use core::panic;

    use super::*;

    #[test]
    fn test_block_line() {
        let line = parse_line("section:");
        let condition = Line::Block {
            indent: 0,
            name: "section",
            attributes: None,
            flow: None,
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_block_attributes_line() {
        let line = parse_line(r#"section:(class="hi")"#);
        let condition = Line::Block {
            indent: 0,
            name: "section",
            attributes: Some(vec![Attribute::Pair {
                key: "class",
                value: r#""hi""#,
            }]),
            flow: None,
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_block_content_line() {
        let line = parse_line(r#"section: The Title"#);
        let condition = Line::Block {
            indent: 0,
            name: "section",
            attributes: None,
            flow: Some(Flow::from("The Title")),
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_block_atrributes_content_line() {
        let line = parse_line(r#"section:(class="hi",special) The Title"#);
        let condition = Line::Block {
            indent: 0,
            name: "section",
            attributes: Some(vec![
                Attribute::Pair {
                    key: "class",
                    value: r#""hi""#,
                },
                Attribute::Tag("special"),
            ]),
            flow: Some(Flow::from("The Title")),
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_block_atrributes_content_line_indent() {
        let line = parse_line(r#"		section:(class="hi") The Title"#);
        let condition = Line::Block {
            indent: 2,
            name: "section",
            attributes: Some(vec![Attribute::Pair {
                key: "class",
                value: r#""hi""#,
            }]),
            flow: Some(Flow::from("The Title")),
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_ordered_list_line() {
        let line = parse_line("1. Hello");
        let condition = Line::OrderedListItem {
            indent: 0,
            flow: Flow::from("Hello"),
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_ordered_list_line_indent() {
        let line = parse_line("			1. Hello");
        let condition = Line::OrderedListItem {
            indent: 3,
            flow: Flow::from("Hello"),
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_unordered_list_line() {
        let line = parse_line("* Hello");
        let condition = Line::UnorderedListItem {
            indent: 0,
            flow: Flow::from("Hello"),
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_unordered_list_line_indent() {
        let line = parse_line("			* Hello");
        let condition = Line::UnorderedListItem {
            indent: 3,
            flow: Flow::from("Hello"),
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_paragraph_line() {
        let line = parse_line("this is a paragraph");
        let condition = Line::Paragraph {
            indent: 0,
            flow: Flow::from("this is a paragraph"),
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_paragraph_line_indent() {
        let line = parse_line("	this is a paragraph");
        let condition = Line::Paragraph {
            indent: 1,
            flow: Flow::from("this is a paragraph"),
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_parse() {
        let mut element = parse(r#"source:(hello="world") Header"#).unwrap();
        let element = match element.children.pop().unwrap() {
            Child::Element(element) => element,
            _ => panic!(),
        };
        let condition = Element {
            name: "source",
            attributes: Some(vec![Attribute::Pair {
                key: "hello",
                value: r#""world""#,
            }]),
            flow: Some(Flow(vec!["Header"])),
            children: vec![],
        };
        assert_eq!(element, condition);
    }
}
