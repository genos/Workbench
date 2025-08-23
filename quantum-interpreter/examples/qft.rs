use quantum_interpreter::{Gate, Instruction, Kind, Machine, Program};

fn bit_rev(qs: &[u32]) -> Vec<Instruction> {
    let n = qs.len() / 2;
    qs.iter()
        .zip(qs.iter().rev())
        .take(n)
        .map(|(&a, &b)| {
            Instruction::Gate(Gate::TwoQ {
                kind: Kind::Swap,
                qubits: (a, b),
            })
        })
        .collect()
}
fn qft_helper(qs: &[u32]) -> Vec<Instruction> {
    match qs.split_first() {
        None => vec![],
        Some((h, [])) => vec![Instruction::Gate(Gate::OneQ {
            kind: Kind::H,
            qubit: *h,
        })],
        Some((h, ts)) => qft_helper(ts)
            .into_iter()
            .chain(ts.iter().enumerate().map(|(i, t)| {
                let angle = 2f64.powi(i32::try_from(ts.len() - i).expect("Should be small enough"));
                Instruction::Gate(Gate::TwoQ {
                    kind: Kind::Cphase(angle),
                    qubits: (*h, *t),
                })
            }))
            .chain(std::iter::once(Instruction::Gate(Gate::OneQ {
                kind: Kind::H,
                qubit: *h,
            })))
            .collect(),
    }
}

fn qft(qubits: &[u32]) -> Program {
    let mut instructions = qft_helper(qubits);
    instructions.append(&mut bit_rev(qubits));
    instructions.push(Instruction::Measure);
    Program { instructions }
}

fn main() -> Result<(), String> {
    let mut machine = Machine::new(4, 2_718_281_828);
    let prog = qft(&[0, 1, 2, 3]);
    let mut counts = vec![0; 16];
    for _ in 0..1000 {
        counts[machine.run(&prog)? as usize] += 1;
    }
    println!("{counts:?}");
    Ok(())
}
