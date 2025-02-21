use quantum_interpreter::{Machine, Program};
use std::str::FromStr;

static PROGRAM: &str = r"
H 0
CNOT 0 1
MEASURE
";

fn main() -> Result<(), String> {
    let mut machine = Machine::new(2, 1729)?;
    let prog = Program::from_str(PROGRAM)?;
    let mut counts = vec![0; 4];
    for _ in 0..1000 {
        counts[machine.run(&prog)?] += 1;
    }
    println!("{counts:?}");
    Ok(())
}
