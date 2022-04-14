use lazy_static::lazy_static;
use regex::Regex;
use std::fmt::Display;
use thiserror::Error;

pub enum Element<'a> {
    Named {
        name: &'a str,
        text_content: Option<Box<Element<'a>>>,
        children: Vec<Element<'a>>,
    },
    CharacterData(&'a str),
}
impl<'a> Display for Element<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let out = match self {
            Element::Named { name, .. } => format!("Named( {} )", name),
            Element::CharacterData(content) => format!("CharacterData( {} )", content),
        };
        write!(f, "{}", out)
    }
}

pub struct Tree {
    pub root: Element<'static>,
    source: String,
}
impl Tree {
    pub fn parse(source: String) -> Tree {
        println!("\n===== begin parse =====");

        parse(&source);

        println!("===== end parse =====\n");

        Tree {
            root: Element::Named {
                name: "hello",
                text_content: None,
                children: vec![],
            },
            source,
        }
    }
    pub fn to_xml(&self) -> String {
        String::from("xml :)")
    }
}

#[derive(Debug, Error)]
pub(crate) enum ParserError {
    #[error("Invalid line break")]
    IllegalLineBreak,

    #[error("Tried to indent more than one level at once")]
    MoreThanOneIndent,
}

fn parse<'a>(source: &'a str) {
    let mut lines = source.lines().peekable();
    while let Some(line) = lines.next() {
        let line = parse_line(line);
        println!("{:?}", line);
    }
}

#[derive(PartialEq, Debug)]
enum Line<'a> {
    Block {
        indent: u16,
        name: &'a str,
        attributes: Vec<(&'a str, &'a str)>,
        flow: Option<&'a str>,
    },
    OrderedListItem {
        indent: u16,
        flow: &'a str,
    },
    UnorderedListItem {
        indent: u16,
        flow: &'a str,
    },
    Paragraph {
        indent: u16,
        flow: &'a str,
    },
    Blank,
}
fn parse_line<'a>(line: &'a str) -> Line<'a> {
    if line.trim().is_empty() {
        return Line::Blank;
    }

    let (indent, text) = parse_indentations(line);

    if let Some(block_line) = parse_block_line(indent, text) {
        return block_line;
    }
    if let Some(ordered_list_line) = parse_ordered_list_item(indent, text) {
        return ordered_list_line;
    }
    if let Some(unordered_list_line) = parse_unordered_list_item(indent, text) {
        return unordered_list_line;
    }

    // else its a paragraph
    Line::Paragraph { indent, flow: text }
}

fn parse_block_line<'a>(indent: u16, text: &'a str) -> Option<Line> {
    // check for and split on required colon
    let (name_candidate, remainder) = match text.split_once(":") {
        Some((n, r)) => (n, r),
        None => return None, // abort if no colon
    };

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
        None => Vec::new(),
    };

    // parse flow string
    let flow = match flow.is_empty() {
        false => match flow.strip_prefix(" ") {
            Some(flow) => Some(flow),
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

    return Some(Line::UnorderedListItem { indent, flow });
}

fn parse_attributes(attributes_string: &str) -> Vec<(&str, &str)> {
    let mut attributes = Vec::new();
    for a in attributes_string.split(",").collect::<Vec<_>>() {
        if let Some((property, value)) = a.split_once("=") {
            attributes.push((property, value));
        } else {
            // malformed attribute?? no, doesn't need an equals
        }
    }
    attributes
}

fn parse_indentations<'a>(line: &str) -> (u16, &str) {
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
    use super::*;

    #[test]
    fn test_block_line() {
        let line = parse_line("section:");
        let condition = Line::Block {
            indent: 0,
            name: "section",
            attributes: vec![],
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
            attributes: vec![("class", r#""hi""#)],
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
            attributes: vec![],
            flow: Some("The Title"),
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_block_atrributes_content_line() {
        let line = parse_line(r#"section:(class="hi") The Title"#);
        let condition = Line::Block {
            indent: 0,
            name: "section",
            attributes: vec![("class", r#""hi""#)],
            flow: Some("The Title"),
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_block_atrributes_content_line_indent() {
        let line = parse_line(r#"		section:(class="hi") The Title"#);
        let condition = Line::Block {
            indent: 2,
            name: "section",
            attributes: vec![("class", r#""hi""#)],
            flow: Some("The Title"),
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_ordered_list_line() {
        let line = parse_line("1. Hello");
        let condition = Line::OrderedListItem {
            indent: 0,
            flow: "Hello",
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_ordered_list_line_indent() {
        let line = parse_line("			1. Hello");
        let condition = Line::OrderedListItem {
            indent: 3,
            flow: "Hello",
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_unordered_list_line() {
        let line = parse_line("* Hello");
        let condition = Line::UnorderedListItem {
            indent: 0,
            flow: "Hello",
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_unordered_list_line_indent() {
        let line = parse_line("			* Hello");
        let condition = Line::UnorderedListItem {
            indent: 3,
            flow: "Hello",
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_paragraph_line() {
        let line = parse_line("this is a paragraph");
        let condition = Line::Paragraph {
            indent: 0,
            flow: "this is a paragraph",
        };
        assert_eq!(line, condition);
    }

    #[test]
    fn test_paragraph_line_indent() {
        let line = parse_line("	this is a paragraph");
        let condition = Line::Paragraph {
            indent: 1,
            flow: "this is a paragraph",
        };
        assert_eq!(line, condition);
    }
}
