use quantum_interpreter::{Gate, Instruction, Machine, Program};

fn bit_rev(qs: &[usize]) -> Vec<Instruction> {
    let n = qs.len() / 2;
    qs.iter()
        .zip(qs.iter().rev())
        .take(n)
        .map(|(&a, &b)| Instruction::Gate {
            gate: Gate::Swap,
            qubits: vec![a, b],
        })
        .collect()
}
fn qft_helper(qs: &[usize]) -> Vec<Instruction> {
    match qs.split_first() {
        None => vec![],
        Some((h, [])) => vec![Instruction::Gate {
            gate: Gate::H,
            qubits: vec![*h],
        }],
        Some((h, ts)) => qft_helper(ts)
            .into_iter()
            .chain(ts.iter().enumerate().map(|(i, t)| {
                let angle = 2f64.powi(i32::try_from(ts.len() - i).expect("Should be small enough"));
                Instruction::Gate {
                    gate: Gate::Cphase(angle),
                    qubits: vec![*h, *t],
                }
            }))
            .chain(std::iter::once(Instruction::Gate {
                gate: Gate::H,
                qubits: vec![*h],
            }))
            .collect(),
    }
}

fn qft(qubits: &[usize]) -> Program {
    let mut instructions = qft_helper(qubits);
    instructions.append(&mut bit_rev(qubits));
    instructions.push(Instruction::Measure);
    Program { instructions }
}

fn main() -> Result<(), String> {
    let mut machine = Machine::new(4, 2_718_281_828)?;
    let prog = qft(&[0, 1, 2, 3]);
    let mut counts = vec![0; 16];
    for _ in 0..1000 {
        counts[machine.run(&prog)?] += 1;
    }
    println!("{counts:?}");
    Ok(())
}
