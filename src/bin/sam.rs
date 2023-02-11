use anyhow::{Context, Result};
use clap::Parser;
use sam_rs::Element;
use std::path::{Path, PathBuf};

#[derive(Parser)]
#[clap(name = "sam-rs")]
#[clap(author = "@jakintosh")]
#[clap(version = "0.1.0")]
#[clap(about = "semantic authoring markdown parser", long_about = None)]
struct Args {
    /// Input SAM path to be parsed
    source: String,

    /// Output file path
    output: String,

    /// Treat source/output as directories
    #[clap(short, long)]
    batch: bool,

    /// Output to stdin instead of output file
    #[clap(short, long)]
    dryrun: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();
    match args.batch {
        true => parse_batch(args.source, args.output, args.dryrun),
        false => parse_file(args.source, args.output, args.dryrun),
    }
}

fn parse_batch(in_path: String, out_path: String, dryrun: bool) -> Result<()> {
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

fn parse_file(in_path: String, out_path: String, dryrun: bool) -> Result<()> {
    let input = std::fs::read_to_string(&in_path).context("Couldn't load source file")?;
    let root: Element = input
        .parse()
        .context(format!("{}: Failed to parse input", in_path))?;
    let xml = root.to_xml(0, false);

    match dryrun {
        true => println!("\n\nFile: {}\n\n{}", out_path, xml),
        false => {
            let out = PathBuf::try_from(out_path).context("couldn't build path")?;
            std::fs::create_dir_all(out.parent().unwrap()).context("couldn't ensure dir path")?;
            std::fs::write(&out, &xml)?
        }
    };

    Ok(())
}

fn read_directory<P: AsRef<Path>>(path: P, paths: &mut Vec<String>) -> Result<()> {
    let dir = std::fs::read_dir(path).context("couldn't read dir")?;
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
