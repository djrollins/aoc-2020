use itertools::iproduct;

use aoc_2020::utils::read_input;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let contents: String = read_input()?;

    let input: Vec<i32> = contents.lines().map(|line| line.parse().unwrap()).collect();

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
