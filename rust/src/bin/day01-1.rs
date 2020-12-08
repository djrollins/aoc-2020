use itertools::iproduct;

use aoc_2020::utils::read_input;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let contents = read_input()?;

    let input: Vec<i32> = contents.lines().map(|line| line.parse().unwrap()).collect();

    let answer = iproduct!(input.iter().cloned(), input.iter().cloned())
        .find(|(x, y)| x + y == 2020)
        .map(|(x, y)| x * y)
        .unwrap();

    println!("{}", answer);

    Ok(())
}
