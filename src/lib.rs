use lazy_static::lazy_static;
use regex::Regex;
use std::{fmt::Display, str::FromStr};
use thiserror::Error;

/*
    Element components
*/

// Attribute
#[derive(PartialEq, Debug, Clone)]
pub enum Attribute {
    Tag(String),
    Pair { key: String, value: String },
}
impl Display for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Attribute::Tag(tag) => write!(f, "{}", tag),
            Attribute::Pair { key, value } => write!(f, "{}={}", key, value),
        }
    }
}

// Flow
#[derive(PartialEq, Debug, Clone)]
pub struct Flow(String);
impl From<&str> for Flow {
    fn from(s: &str) -> Self {
        Flow(String::from(s))
    }
}
impl Flow {
    pub(crate) fn append(&mut self, flow: &mut Flow) {
        self.0.push_str(&mut flow.0)
    }
    pub(crate) fn into_elements(&self) -> Vec<Child> {
        let mut elements = Vec::new();
        let mut flow = self.0.clone();

        while let Some(ContainedString {
            before,
            inner,
            mut after,
        }) = parse_contained_string(flow.as_str(), "{", "}")
        {
            // before is char data
            elements.push(Child::CharacterData(before));

            // inner is an annotated phrase
            // build a vec of annotations
            let mut annotations = Vec::new();
            while let (Some(annotation), next) = parse_immediate_parentheses(&after) {
                let annotation = Annotation::parse(&annotation);
                annotations.push(annotation);
                after = String::from(next);
            }
            // unwind the annotation stack and wrap the inner in them
            let mut element = Child::CharacterData(inner);
            while let Some(annotation) = annotations.pop() {
                let mut parent = Element::new(&annotation.name, annotation.attributes, None);
                parent.flow.push(element);
                element = Child::Element(parent);
            }
            elements.push(element);

            // remainder is parsed again
            flow = after;
        }

        // push last bit of unprocessed flow
        if !flow.is_empty() {
            elements.push(Child::CharacterData(flow));
        }

        elements
    }
}

// Annotation
struct Annotation {
    name: String,
    attributes: Option<Vec<Attribute>>,
}
impl Annotation {
    fn parse(s: &str) -> Annotation {
        let (name, attributes) = match s.split_once("|") {
            Some((name, attributes)) => (String::from(name), parse_attributes(attributes)),
            None => (String::from(s), None),
        };
        Annotation { name, attributes }
    }
}

// Child
#[derive(PartialEq, Debug)]
pub enum Child {
    Element(Element),
    CharacterData(String),
}
impl Child {
    pub(crate) fn to_xml(&self, indent: usize, parent_is_flow: bool) -> String {
        match self {
            Child::Element(e) => e.to_xml(indent, parent_is_flow),
            Child::CharacterData(s) => s.clone(),
        }
    }
}

// Element
#[derive(PartialEq, Debug)]
pub struct Element {
    name: String,
    attributes: Option<Vec<Attribute>>,
    flow: Vec<Child>,
    blocks: Vec<Child>,
}
impl FromStr for Element {
    type Err = ParserError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut lines = s.lines().peekable();
        let root = Element::new("@parse-root", None, None);
        let mut stack = Stack::new(root);
        let mut line_num = 0;
        while let Some(line) = lines.next() {
            line_num += 1;
            let line = line.parse::<Line>()?;
            let indents = line.get_indents();
            let element = match line {
                Line::Block {
                    name,
                    attributes,
                    flow,
                    ..
                } => Element::new(&name, attributes, flow),

                Line::OrderedListItem {
                    indent: base_indent,
                    flow,
                } => {
                    let mut ol = Element::new("ol", None, None);
                    ol.add_child_element(Element::new("li", None, Some(flow)));
                    while let Some(peek) = lines.peek() {
                        match peek.parse()? {
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
                        match peek.parse()? {
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
                        match peek.parse()? {
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
                Line::LiteralFlag {
                    indent: base_indent,
                } => {
                    let mut literal = String::new();
                    while let Some(line) = lines.next() {
                        line_num += 1;
                        match line.parse()? {
                            Line::LiteralFlag { indent } => {
                                if indent != base_indent {
                                    return Err(ParserError::IllegalLiteralFlagIndentation {
                                        line: line_num,
                                    });
                                }
                                match literal.strip_suffix('\n') {
                                    Some(literal) => {
                                        stack.push_child(Child::CharacterData(literal.to_string()))
                                    }
                                    None => panic!("literal flag close should always end with \\n"),
                                }

                                break;
                            }
                            _ => match trim_indentations(line, base_indent) {
                                Some(s) => {
                                    literal.push_str(s);
                                    literal.push('\n');
                                }
                                None => {
                                    return Err(ParserError::IllegalLiteralBlockIndentation {
                                        line: line_num,
                                    });
                                }
                            },
                        }
                    }
                    continue;
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
}
impl Element {
    pub fn to_xml(&self, indent: usize, parent_is_flow: bool) -> String {
        let next_indent = match self.name.as_str() {
            "@parse-root" => 0,
            _ => indent + 1,
        };
        let tabs = "\t".repeat(indent);
        let name = self.name.clone();
        let attributes = match &self.attributes {
            Some(attrs) => attrs
                .iter()
                .map(|a| format!(" {}", a))
                .collect::<Vec<_>>()
                .join(""),
            None => String::from(""),
        };
        let flows = self
            .flow
            .iter()
            .map(|c| c.to_xml(next_indent, true))
            .collect::<Vec<_>>()
            .join("");
        let blocks = self
            .blocks
            .iter()
            .map(|c| c.to_xml(next_indent, false))
            .collect::<Vec<_>>()
            .join("\n");

        if name == "@parse-root" {
            format!("{}", blocks)
        } else {
            if !blocks.is_empty() {
                if !flows.is_empty() {
                    // flow + block
                    format!(
                        "{tabs}<{name}{attr}>\n{tabs}\t<h1>{flow}</h1>\n{bloc}\n{tabs}</{name}>",
                        tabs = tabs,
                        name = name,
                        attr = attributes,
                        flow = flows,
                        bloc = blocks,
                    )
                } else {
                    // only block
                    format!(
                        "{tabs}<{name}{attr}>\n{bloc}\n{tabs}</{name}>",
                        tabs = tabs,
                        name = name,
                        attr = attributes,
                        bloc = blocks
                    )
                }
            } else {
                if !flows.is_empty() {
                    // only flow
                    if parent_is_flow {
                        format!(
                            "<{name}{attr}>{flow}</{name}>",
                            name = name,
                            flow = flows,
                            attr = attributes
                        )
                    } else {
                        if name == "p" {
                            format!(
                                "{tabs}<{name}{attr}>\n{tabs}\t{flow}\n{tabs}</{name}>",
                                name = name,
                                flow = flows,
                                attr = attributes
                            )
                        } else {
                            format!(
                                "{tabs}<{name}{attr}>{flow}</{name}>",
                                tabs = tabs,
                                name = name,
                                flow = flows,
                                attr = attributes
                            )
                        }
                    }
                } else {
                    // niether
                    format!(
                        "{tabs}<{name}{attr}/>",
                        tabs = tabs,
                        name = name,
                        attr = attributes
                    )
                }
            }
        }
    }

    pub(crate) fn new(
        name: &str,
        attributes: Option<Vec<Attribute>>,
        flow: Option<Flow>,
    ) -> Element {
        let name = String::from(name);
        let flow = match flow {
            Some(flow) => flow.into_elements(),
            None => Vec::new(),
        };
        Element {
            name,
            attributes,
            flow,
            blocks: Vec::new(),
        }
    }
    pub(crate) fn add_child_element(&mut self, child: Element) {
        self.blocks.push(Child::Element(child))
    }
    pub(crate) fn add_child_text(&mut self, text: String) {
        self.blocks.push(Child::CharacterData(text))
    }
}

/*
    Parsing
*/

// Line
#[derive(PartialEq, Debug)]
enum Line {
    Block {
        indent: u16,
        name: String,
        attributes: Option<Vec<Attribute>>,
        flow: Option<Flow>,
    },
    OrderedListItem {
        indent: u16,
        flow: Flow,
    },
    UnorderedListItem {
        indent: u16,
        flow: Flow,
    },
    Paragraph {
        indent: u16,
        flow: Flow,
    },
    LiteralFlag {
        indent: u16,
    },
    Blank,
}
impl FromStr for Line {
    type Err = ParserError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (indent, text) = split_indentations(s);
        if s.trim().is_empty() {
            Ok(Line::Blank)
        } else if text == "```" {
            Ok(Line::LiteralFlag { indent })
        } else if let Some(block_line) = try_parse_block_line(indent, text) {
            Ok(block_line)
        } else if let Some(ordered_list_line) = try_parse_ordered_list_line(indent, text) {
            Ok(ordered_list_line)
        } else if let Some(unordered_list_line) = try_parse_unordered_list_line(indent, text) {
            Ok(unordered_list_line)
        } else {
            let flow = text.into();
            Ok(Line::Paragraph { indent, flow })
        }
    }
}
impl Line {
    pub(crate) fn get_indents(&self) -> Option<u16> {
        match self {
            Line::Block { indent, .. }
            | Line::OrderedListItem { indent, .. }
            | Line::UnorderedListItem { indent, .. }
            | Line::Paragraph { indent, .. }
            | Line::LiteralFlag { indent } => Some(*indent),
            Line::Blank => None,
        }
    }
}

// Parsing Stack
struct Stack {
    stack: Vec<Element>,
}
impl Stack {
    pub(crate) fn new(root: Element) -> Stack {
        Stack { stack: vec![root] }
    }
    pub(crate) fn get_indent_delta(&self, indent: u16) -> i32 {
        (indent as i32) - (self.stack.len() as i32 - 2)
    }
    pub(crate) fn open(&mut self, element: Element) {
        self.stack.push(element);
    }
    pub(crate) fn push_child(&mut self, child: Child) {
        let last = match self.stack.last_mut() {
            Some(l) => l,
            None => todo!(),
        };
        match child {
            Child::Element(e) => last.add_child_element(e),
            Child::CharacterData(text) => last.add_child_text(text),
        }
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
    pub(crate) fn finalize(&mut self) -> Element {
        while self.stack.len() > 1 {
            self.close();
        }
        match self.stack.pop() {
            Some(root) => root,
            None => todo!(),
        }
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

    #[error("Line {line}: Opening and closing literal flags must have the same indentation")]
    IllegalLiteralFlagIndentation { line: usize },

    #[error("Line {line}: All lines in literal block must be <= indentation than root")]
    IllegalLiteralBlockIndentation { line: usize },

    #[error("Line {line}: Invalid child nested; paragraphs and list items cannot have children")]
    IllegalChild { line: usize },

    #[error("Line {line}: Tried to indent {indents} levels at once; maximum one at a time")]
    MoreThanOneIndent { line: usize, indents: i32 },
}

fn split_indentations(line: &str) -> (u16, &str) {
    let mut line = line;
    let mut indents = 0;
    while let Some(substring) = line.strip_prefix("\t") {
        line = substring;
        indents += 1;
    }
    (indents, line)
}

fn trim_indentations(line: &str, indent: u16) -> Option<&str> {
    match line.is_empty() {
        true => Some(line), // if empty, stay empty
        false => line.strip_prefix(&"\t".repeat(indent as usize)),
    }
}

fn try_parse_block_line(indent: u16, text: &str) -> Option<Line> {
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
        true => String::from(name_candidate),
        false => return None, // abort if invalid name
    };

    // split attributes and flow
    let (attributes_string, flow) = parse_immediate_parentheses(remainder);

    // parse attributes string
    let attributes = match attributes_string {
        Some(attributes_string) => parse_attributes(&attributes_string),
        None => None,
    };

    // parse flow string
    let flow = match flow.is_empty() {
        false => match flow.strip_prefix(" ") {
            Some(flow) => Some(flow.into()),
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

fn try_parse_ordered_list_line(indent: u16, text: &str) -> Option<Line> {
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

    let flow = flow.into();

    return Some(Line::OrderedListItem { indent, flow });
}

fn try_parse_unordered_list_line(indent: u16, text: &str) -> Option<Line> {
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

    let flow = flow.into();

    return Some(Line::UnorderedListItem { indent, flow });
}

fn parse_attributes(attributes_string: &str) -> Option<Vec<Attribute>> {
    let mut attributes = Vec::new();
    for a in attributes_string.split(",").collect::<Vec<_>>() {
        let attribute = match a.split_once("=") {
            Some((key, value)) => Attribute::Pair {
                key: String::from(key),
                value: String::from(value),
            },
            None => Attribute::Tag(String::from(a)),
        };
        attributes.push(attribute);
    }

    match attributes.is_empty() {
        false => Some(attributes),
        true => None,
    }
}

fn parse_immediate_parentheses(line: &str) -> (Option<String>, &str) {
    match line.strip_prefix("(") {
        Some(remaining) => match remaining.split_once(")") {
            Some((left, right)) => (Some(String::from(left)), right),
            None => (None, line),
        },
        None => (None, line),
    }
}

struct ContainedString {
    before: String,
    inner: String,
    after: String,
}
fn parse_contained_string(line: &str, open: &str, close: &str) -> Option<ContainedString> {
    match line.split_once(open) {
        Some((before, after)) => match after.split_once(close) {
            Some((inner, after)) => Some(ContainedString {
                before: String::from(before),
                inner: String::from(inner),
                after: String::from(after),
            }),
            None => None,
        },
        None => None,
    }
}

fn is_valid_name_token(token: &str) -> bool {
    lazy_static! {
        static ref RE: Regex =
            Regex::new(r"^\b[A-Za-z0-9]+(?:['-]?[A-Za-z0-9]+)*\b$").expect("Invalid Regex");
    }
    RE.is_match(token)
}

#[cfg(test)]
mod test {
    use core::panic;

    use super::*;

    #[test]
    fn test_block_line() {
        let line = "section:".parse::<Line>().expect("Failed to parse line");
        let condition = Line::Block {
            indent: 0,
            name: "section".to_string(),
            attributes: None,
            flow: None,
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_block_attributes_line() {
        let line = r#"section:(class="hi")"#.parse::<Line>().expect("Failed to parse line");
        let condition = Line::Block {
            indent: 0,
            name: "section".to_string(),
            attributes: Some(vec![Attribute::Pair {
                key: "class".to_string(),
                value: r#""hi""#.to_string(),
            }]),
            flow: None,
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_block_content_line() {
        let line = r#"section: The Title"#.parse::<Line>().expect("Failed to parse line");
        let condition = Line::Block {
            indent: 0,
            name: "section".to_string(),
            attributes: None,
            flow: Some("The Title".into()),
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_block_atrributes_content_line() {
        let line = r#"section:(class="hi",special) The Title"#
            .parse::<Line>()
            .expect("Failed to parse line");
        let condition = Line::Block {
            indent: 0,
            name: "section".to_string(),
            attributes: Some(vec![
                Attribute::Pair {
                    key: "class".to_string(),
                    value: r#""hi""#.to_string(),
                },
                Attribute::Tag("special".to_string()),
            ]),
            flow: Some("The Title".into()),
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_block_atrributes_content_line_indent() {
        let line = r#"		section:(class="hi") The Title"#
            .parse::<Line>()
            .expect("Failed to parse line");
        let condition = Line::Block {
            indent: 2,
            name: "section".to_string(),
            attributes: Some(vec![Attribute::Pair {
                key: "class".to_string(),
                value: r#""hi""#.to_string(),
            }]),
            flow: Some("The Title".into()),
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_ordered_list_line() {
        let line = "1. Hello".parse::<Line>().expect("Failed to parse line");
        let condition = Line::OrderedListItem {
            indent: 0,
            flow: "Hello".into(),
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_ordered_list_line_indent() {
        let line = "			1. Hello".parse::<Line>().expect("Failed to parse line");
        let condition = Line::OrderedListItem {
            indent: 3,
            flow: "Hello".into(),
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_unordered_list_line() {
        let line = "* Hello".parse::<Line>().expect("Failed to parse line");
        let condition = Line::UnorderedListItem {
            indent: 0,
            flow: "Hello".into(),
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_unordered_list_line_indent() {
        let line = "			* Hello".parse::<Line>().expect("Failed to parse line");
        let condition = Line::UnorderedListItem {
            indent: 3,
            flow: "Hello".into(),
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_paragraph_line() {
        let line = "this is a paragraph"
            .parse::<Line>()
            .expect("Failed to parse line");
        let condition = Line::Paragraph {
            indent: 0,
            flow: "this is a paragraph".into(),
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_paragraph_line_indent() {
        let line = "	this is a paragraph"
            .parse::<Line>()
            .expect("Failed to parse line");
        let condition = Line::Paragraph {
            indent: 1,
            flow: "this is a paragraph".into(),
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_parse() {
        let mut element = r#"source:(hello="world") Header"#
            .parse::<Element>()
            .expect("failed to parse element");
        let element = match element.blocks.pop().unwrap() {
            Child::Element(element) => element,
            _ => panic!(),
        };
        let condition = Element {
            name: "source".to_string(),
            attributes: Some(vec![Attribute::Pair {
                key: "hello".to_string(),
                value: r#""world""#.to_string(),
            }]),
            flow: vec![Child::CharacterData("Header".to_string())],
            blocks: vec![],
        };
        assert_eq!(element, condition);
    }

    #[test]
    fn test_parse_flow() {
        let flow = Flow(String::from(
            r#"Hello {world}(test|attr="good")(test2) goodbye"#,
        ));
        let expect = vec![
            Child::CharacterData("Hello ".to_string()),
            Child::Element(Element {
                name: "test".to_string(),
                attributes: Some(vec![Attribute::Pair {
                    key: "attr".to_string(),
                    value: r#""good""#.to_string(),
                }]),
                flow: vec![Child::Element(Element {
                    name: "test2".to_string(),
                    attributes: None,
                    flow: vec![Child::CharacterData("world".to_string())],
                    blocks: vec![],
                })],
                blocks: vec![],
            }),
            Child::CharacterData(" goodbye".to_string()),
        ];
        let flow = flow.into_elements();
        assert_eq!(flow, expect);
    }

    #[test]
    fn test_name_lowercase() {
        assert!(is_valid_name_token("valid-name"))
    }

    #[test]
    fn test_name_uppercase() {
        assert!(is_valid_name_token("Valid-Name"))
    }

    #[test]
    fn test_name_number() {
        assert!(is_valid_name_token("666-name-666"))
    }
}
