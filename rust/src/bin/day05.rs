use aoc_2020::utils::read_input;
use std::error::Error;
use itertools::Itertools;

fn parse(string: &str) -> u32 {
    let row = string
        .chars()
        .take(7)
        .map(|c| if c == 'B' { 1 } else { 0 })
        .fold(0, |acc, item| (acc << 1) + item);
    let col = string
        .chars()
        .skip(7)
        .map(|c| if c == 'R' { 1 } else { 0 })
        .fold(0, |acc, item| (acc << 1) + item);
    8 * row + col
}

fn main() -> Result<(), Box<dyn Error>> {
    let contents = read_input()?;
    let seats = contents.lines().map(parse).sorted();

    let part_1 = seats.clone().max().unwrap();
    println!("part 1: {}", part_1);
    
    let part_2 = seats.clone().zip(seats.skip(1)).find(|(x, y)| (x + 2) == *y).map(|(x, _)| x + 1).unwrap();
    println!("part 2: {}", part_2);

    Ok(())
}
