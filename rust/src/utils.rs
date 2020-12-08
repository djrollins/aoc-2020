use std::env::{args, current_dir};

pub fn read_input() -> Result<String, Box<dyn std::error::Error>> {
    let filename = args().nth(1).ok_or("plz give filename")?;
    let filename = current_dir()?.join(filename);

    Ok(std::fs::read_to_string(&filename)?)
}
