use itertools::{iproduct, Itertools};
use std::env::{args, current_dir};
use std::io::Read;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let filename = args().nth(1).ok_or("plz give filename")?;
    let filename = current_dir()?.join(filename);
    let mut file = std::fs::File::open(filename)?;

    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let input: Vec<i32> = contents.lines().map(|line| line.parse()).collect();

    let answer = iproduct!(
        input.iter().cloned(),
        input.iter().cloned(),
        input.iter().cloned()
    )
    .find(|(x, y, z)| x + y + z == 2020)
    .map(|(x, y, z)| x * y * z)
    .unwrap();

    println!("{}", answer);

    Ok(())
}
