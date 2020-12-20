use aoc_2020::utils::read_input;
use std::collections::HashSet;
use std::error::Error;

fn parse(string: &str) -> (&str, i32) {
    let split: Vec<&str> = string.split(' ').collect();
    return (split[0], split[1].parse().unwrap());
}

#[derive(Debug)]
enum ExecutionResult {
    Complete(i32),
    Loop(i32),
}

fn execute(instructions: &[(&str, i32)]) -> ExecutionResult {
    let mut iptr: i32 = 0;
    let mut acc = 0;
    let mut history = HashSet::new();

    loop {
        let index = iptr as usize;

        if index == instructions.len() {
            return ExecutionResult::Complete(acc);
        }

        if !history.insert(iptr) {
            return ExecutionResult::Loop(acc);
        }

        match instructions[iptr as usize] {
            ("nop", _) => iptr += 1,
            ("jmp", offset) => iptr += offset,
            ("acc", offset) => {
                acc += offset;
                iptr += 1
            }
            unknown => panic!("unexpected operation {:?}", unknown),
        }
    }
}

fn swap_instruction(instructions: &mut [(&str, i32)], index: usize) {
    let (inst, offset) = instructions[index];
    instructions[index] = if inst == "jmp" {
        ("nop", offset)
    } else {
        ("jmp", offset)
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let contents = read_input()?;
    let mut instructions: Vec<_> = contents.lines().map(parse).collect();

    let acc = execute(&instructions);
    println!("part 1: {:?}", acc);

    let nops_and_jmps: Vec<usize> = instructions
        .iter()
        .enumerate()
        .filter_map(|(i, (op, _))| match *op {
            "nop" | "jmp" => Some(i),
            _ => None,
        })
        .collect();

    let acc = nops_and_jmps.into_iter().find_map(|inst_to_swap| {
        swap_instruction(&mut instructions, inst_to_swap);
        match execute(&instructions) {
            ExecutionResult::Complete(val) => Some(val),
            ExecutionResult::Loop(_) => {
                swap_instruction(&mut instructions, inst_to_swap);
                None
            }
        }
    });

    println!("part 2: {:?}", acc);
    Ok(())
}
