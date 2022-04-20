use anyhow::{Context, Result};
use clap::Parser;
use sam_rs::Element;

#[derive(Parser)]
#[clap(name = "sam-rs")]
#[clap(author = "@jakintosh")]
#[clap(version = "0.1.0")]
#[clap(about = "semantic authoring markdown parser", long_about = None)]
struct Args {
    /// Input SAM file to be parsed
    #[clap(short, long)]
    source: String,

    /// Output file name
    #[clap(short, long)]
    output: Option<String>,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let input = std::fs::read_to_string(args.source).context("Couldn't load source file")?;
    let root: Element = input.parse().context("Failed to parse input")?;
    let xml = root.to_xml(0, false);
    if let Some(out) = args.output {
        std::fs::write(&out, &xml)?;
    } else {
        println!("{}", xml);
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
