use anyhow::{Context, Result};
use clap::Parser;
use sam_rs::Tree;

#[derive(Parser)]
#[clap(name = "sam-rs")]
#[clap(author = "@jakintosh")]
#[clap(version = "0.1.0")]
#[clap(about = "semantic authoring markdown parser", long_about = None)]
struct Args {
    /// Directory where content is sourced from
    #[clap(short, long)]
    source: String,

    /// Directory where the site is built to
    #[clap(short, long)]
    destination: Option<String>,

    /// Build the site in debug mode
    #[clap(long)]
    debug: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let input = std::fs::read_to_string(args.source).context("Couldn't load source file")?;
    let tree = Tree::parse(&input).context("Failed to parse input")?;
    let xml = tree.to_xml();

    println!("{}", xml);

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_sam() {
        let test_file = "./test/example.sam";
        let input = std::fs::read_to_string(test_file).expect("Couldn't load source file");
        let _ = Tree::parse(&input).expect("Failed to parse input");
    }
}
