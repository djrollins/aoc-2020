use aoc_2020::utils::read_input;

fn collisions<Grid, Row>(down: usize, across: usize, grid: Grid) -> usize
where
    Grid: IntoIterator<Item = Row>,
    Row: IntoIterator<Item = bool>,
{
    let multiplier = across as f32 / down as f32;
    grid.into_iter()
        .enumerate()
        .step_by(down)
        .filter_map(|(row_num, row)| row.into_iter().nth((multiplier * row_num as f32) as usize))
        .filter(|&x| x)
        .count()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let contents = read_input()?;

    let grid: Vec<_> = contents
        .lines()
        .map(|line| line.chars().map(|c| c == '#').cycle())
        .collect();

    println!("part 1: {:?}", collisions(1, 3, grid.clone()));

    let part2: usize = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
        .iter()
        .map(|(down, across)| collisions(*down, *across, grid.clone()))
        .product();

    println!("part 2: {}", part2);

    Ok(())
}
