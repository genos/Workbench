use quantum_interpreter::{Gate, Instruction, Machine, Program};

fn ghz(n: usize) -> Program {
    Program {
        instructions: std::iter::once(Instruction::Gate {
            gate: Gate::H,
            qubits: vec![0],
        })
        .chain((0..n - 1).map(|q| Instruction::Gate {
            gate: Gate::CNot,
            qubits: vec![q, q + 1],
        }))
        .chain(std::iter::once(Instruction::Measure))
        .collect(),
    }
}

fn main() -> Result<(), String> {
    let mut machine = Machine::new(3, 42)?;
    let prog = ghz(3);
    let mut counts = vec![0; 8];
    for _ in 0..1000 {
        counts[machine.run(&prog)?] += 1;
    }
    println!("{counts:?}");
    Ok(())
}
