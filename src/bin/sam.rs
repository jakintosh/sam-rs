// use anyhow::{Context, Result};
use sam_rs::Element;
use std::{
    path::{Path, PathBuf},
    process,
};

const HELP: &str = "
collate v1 by @jakintosh

USAGE:
    collate <source_dir> <output_dir> [--verbose | --quiet]";

const VERSION: &str = "
collate v1 by @jakintosh";

enum Parameters {
    Run {
        source: String,
        output: String,
        dry_run: bool,
        batch: bool,
    },
    Help,
    Version,
}
impl TryFrom<std::env::Args> for Parameters {
    type Error = String;

    fn try_from(mut args: std::env::Args) -> Result<Self, Self::Error> {
        args.next(); // skip first arg, bin location

        let source = match args.next() {
            Some(arg) => match arg.as_str() {
                "--help" | "-h" => return Ok(Parameters::Help),
                "--version" => return Ok(Parameters::Version),
                _ => arg,
            },
            None => return Err(String::from("Missing `source_dir` argument")),
        };
        let output = match args.next() {
            Some(arg) => match arg.as_str() {
                "--help" | "-h" => return Ok(Parameters::Help),
                "--version" => return Ok(Parameters::Version),
                _ => arg,
            },
            None => return Err(String::from("Missing `output_dir` argument")),
        };

        let mut dry_run = false;
        let mut batch = false;
        while let Some(arg) = args.next() {
            match arg.as_str() {
                "--batch" | "-b" => batch = true,
                "--dry-run" | "-d" => dry_run = true,
                _ => panic!("unrecognized parameter: {}\n{}", arg, HELP),
            }
        }

        Ok::<Parameters, String>(Parameters::Run {
            source,
            output,
            dry_run,
            batch,
        })
    }
}

fn main() {
    let parameters: Parameters = match std::env::args().try_into() {
        Ok(params) => params,
        Err(e) => {
            println!("{}\n{}", e, HELP);
            return;
        }
    };
    let (source, output, dry_run, batch) = match parameters {
        Parameters::Run {
            source,
            output,
            dry_run,
            batch,
        } => (source, output, dry_run, batch),
        Parameters::Help => {
            println!("{}", HELP);
            return;
        }
        Parameters::Version => {
            println!("{}", VERSION);
            return;
        }
    };
    let result = match batch {
        true => parse_batch(source, output, dry_run),
        false => parse_file(source, output, dry_run),
    };
    if let Err(e) = result {
        println!("Parse failure: {}", e);
        process::exit(1)
    }
}

fn parse_batch(in_path: String, out_path: String, dryrun: bool) -> Result<(), String> {
    let mut paths = Vec::new();
    read_directory(in_path.clone(), &mut paths)?;
    for path in paths {
        let relative = path.strip_prefix(&in_path).unwrap();
        let mut output = out_path.clone();
        output.push_str(relative);
        parse_file(path, output, dryrun)?;
    }

    Ok(())
}

fn parse_file(in_path: String, out_path: String, dryrun: bool) -> Result<(), String> {
    let input = match std::fs::read_to_string(&in_path) {
        Ok(i) => i,
        Err(e) => return Err(format!("Couldn't read source file: {}", e)),
    };
    let root: Element = match input.parse() {
        Ok(r) => r,
        Err(e) => return Err(format!("Parse error for '{}': {}", in_path, e)),
    };
    let xml = root.to_xml(0, false);

    match dryrun {
        true => println!("\n\nFile: {}\n\n{}", out_path, xml),
        false => {
            let out = match PathBuf::try_from(out_path) {
                Ok(o) => o,
                Err(e) => return Err(format!("Invalid path: {}", e)),
            };
            match std::fs::create_dir_all(out.parent().unwrap()) {
                Ok(_) => {}
                Err(e) => return Err(format!("Failed to create output directories: {}", e)),
            };
            match std::fs::write(&out, &xml) {
                Ok(_) => {}
                Err(e) => return Err(format!("Failed to write output: {}", e)),
            }
        }
    };

    Ok(())
}

fn read_directory<P: AsRef<Path>>(path: P, paths: &mut Vec<String>) -> Result<(), String> {
    let dir = match std::fs::read_dir(path) {
        Ok(d) => d,
        Err(e) => return Err(format!("Couldn't read source directory: {}", e)),
    };
    let entries: Vec<_> = dir.filter_map(|e| e.ok()).collect();
    for e in entries {
        if e.path().starts_with(".") {
            continue;
        }
        if let Ok(filetype) = e.file_type() {
            if filetype.is_dir() {
                read_directory(e.path(), paths)?;
            } else if filetype.is_file() {
                let path = e.path();
                let path = path.as_os_str().to_string_lossy();
                paths.push(path.into());
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_sam() {
        let test_file = "./test/test.sam";
        let input = std::fs::read_to_string(test_file).expect("Couldn't load source file");
        let tree: Element = input.parse().expect("Failed to parse input");
        tree.to_xml(0, false);
    }
}
