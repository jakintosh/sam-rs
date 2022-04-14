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

    // determine if block
    if let Some((block_name_candidate, remainder)) = text.split_once(":") {
        if is_valid_name_token(block_name_candidate) {
            let name = block_name_candidate;
            let mut attributes: Vec<(&'a str, &'a str)> = Vec::new();

            let attributes_candidate = remainder;
            let mut flow_candidate = remainder;

            // parse an attribute candidate
            if let Some(attributes_candidate) = attributes_candidate.strip_prefix("(") {
                if let Some((attributes_string, remainder)) = attributes_candidate.split_once(")") {
                    flow_candidate = remainder;
                    for attribute_string in attributes_string.split(",").collect::<Vec<_>>() {
                        if let Some((property, value)) = attribute_string.split_once("=") {
                            attributes.push((property, value));
                        } else {
                            // malformed attribute?? no, doesn't need an equals
                        }
                    }
                }
            }

            // parse flow candidate
            let flow = match flow_candidate.is_empty() {
                false => flow_candidate.strip_prefix(" "), // None if missing prefix
                true => None,
            };

            return Line::Block {
                indent,
                name,
                attributes,
                flow,
            };
        }
    }

    // determine if ordered list
    if let Some((number_candidate, remainder)) = text.split_once(".") {
        // make sure string before dot is number
        if number_candidate.parse::<u32>().is_ok() {
            // make sure there's a space after the dot
            if let Some(flow) = remainder.strip_prefix(" ") {
                return Line::OrderedListItem { indent, flow };
            }
        }
    }

    // determine if unordered list
    if let Some((asterisk_candidate, flow)) = text.split_once(" ") {
        if asterisk_candidate.eq("*") {
            return Line::UnorderedListItem { indent, flow };
        }
    }

    // else its a paragraph
    Line::Paragraph { indent, flow: text }
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
    let re = match Regex::new(r"\b[a-z]+(?:['-]?[a-z]+)*\b") {
        Ok(re) => re,
        Err(err) => panic!("Invalid Regex"),
    };

    re.is_match(token)
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
