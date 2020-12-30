use aoc_2020::utils::read_input;
use std::error::Error;
use std::fmt::Display;
use std::ops::{Add, AddAssign};

#[derive(Clone, PartialEq, Eq, Debug)]
enum Space {
    Seat(bool),
    Empty,
    Boundary,
}

#[derive(PartialEq, Eq, Clone, Copy)]
struct Coord {
    x: isize,
    y: isize,
}

impl Add for Coord {
    type Output = Coord;

    fn add(self, other: Coord) -> Self::Output {
        Coord {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl AddAssign for Coord {
    fn add_assign(&mut self, other: Coord) {
        let new = *self + other;
        *self = new
    }
}

const DIRECTIONS: [Coord; 8] = [
    Coord { x: -1, y: -1 },
    Coord { x: -1, y: 0 },
    Coord { x: -1, y: 1 },
    Coord { x: 0, y: -1 },
    Coord { x: 0, y: 1 },
    Coord { x: 1, y: -1 },
    Coord { x: 1, y: 0 },
    Coord { x: 1, y: 1 },
];

#[derive(Clone, PartialEq, Eq, Debug)]
struct Layout {
    grid: Vec<Vec<Space>>,
}

impl Display for Layout {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
    ) -> std::result::Result<(), std::fmt::Error> {
        for row in &self.grid {
            let string: String = row
                .iter()
                .map(|space| match space {
                    Space::Seat(true) => '#',
                    Space::Seat(false) => 'L',
                    Space::Empty => '.',
                    Space::Boundary => unreachable!(),
                })
                .collect();
            writeln!(formatter, "{}", string)?
        }

        Ok(())
    }
}

fn occupied_neighbours_part1(layout: &Layout, coord: Coord) -> usize {
    DIRECTIONS
        .iter()
        .map(|offset| layout.at(coord + *offset))
        .filter(|space| *space == Space::Seat(true))
        .count()
}

fn occupied_neighbours_part2(layout: &Layout, coord: Coord) -> usize {
    let mut count = 0;
    for direction in &DIRECTIONS {
        let mut space = direction.clone();
        count += loop {
            let neighbour = layout.at(coord + space);
            if neighbour == Space::Empty {
                space += *direction;
            } else if neighbour == Space::Seat(true) {
                break 1;
            } else {
                break 0;
            }
        }
    }
    count
}

impl Layout {
    fn at(&self, coord: Coord) -> Space {
        let x = coord.x;
        let y = coord.y;

        if x < 0
            || y < 0
            || x >= self.grid.len() as isize
            || y >= self.grid[x as usize].len() as isize
        {
            return Space::Boundary;
        }

        self.grid[x as usize][y as usize].clone()
    }

    fn next_iteration<F>(&self, occupied_neighbours: F, max_neighbours: usize) -> Layout
    where
        F: Fn(&Layout, Coord) -> usize,
    {
        let mut grid = Vec::new();
        for x in 0..self.grid.len() {
            let mut row = Vec::new();
            for y in 0..self.grid[x].len() {
                let coord = Coord {
                    x: x as isize,
                    y: y as isize,
                };
                let current = self.at(coord).clone();
                let occupied_neighbours = occupied_neighbours(self, coord);

                if current == Space::Seat(false) && occupied_neighbours == 0 {
                    row.push(Space::Seat(true));
                } else if current == Space::Seat(true) && occupied_neighbours >= max_neighbours {
                    row.push(Space::Seat(false));
                } else {
                    row.push(current);
                }
            }
            grid.push(row);
        }
        Layout { grid }
    }

    fn total_occupied(&self) -> usize {
        self.grid
            .iter()
            .flat_map(|row| row.iter())
            .filter(|space| **space == Space::Seat(true))
            .count()
    }
}

fn parse(string: &str) -> Vec<Space> {
    string
        .chars()
        .map(|c| {
            if c == 'L' {
                Space::Seat(false)
            } else {
                Space::Empty
            }
        })
        .collect()
}

fn run<F>(layout: &Layout, neighbours_fun: F, max_neighbours: usize) -> usize
where
    F: Fn(&Layout, Coord) -> usize,
{
    let mut layout = layout.clone();
    let mut next_layout = layout.next_iteration(&neighbours_fun, max_neighbours);

    while layout != next_layout {
        layout = next_layout;
        next_layout = layout.next_iteration(&neighbours_fun, max_neighbours);
    }

    return layout.total_occupied();
}

fn main() -> Result<(), Box<dyn Error>> {
    let contents = read_input()?;
    let grid: Vec<Vec<Space>> = contents.lines().map(parse).collect();

    let layout = Layout { grid };
    println!("part 1: {}", run(&layout, occupied_neighbours_part1, 4));
    println!("part 2: {}", run(&layout, occupied_neighbours_part2, 5));

    Ok(())
}
