//! quantum-interpreter
//!
//! A Rust implementation of `stylewarning's` _wonderful_ [tutorial quantum
//! interpreter](https://www.stylewarning.com/posts/quantum-interpreter/), with some inspiration
//! taken from [this OCaml version](https://github.com/sheganinans/QVM-ocaml-mini).

#![deny(clippy::all)]
#![warn(clippy::pedantic)]
#![warn(clippy::nursery)]
#![deny(missing_docs)]
#![deny(unsafe_code)]
use faer::stats::prelude::{Rng, SeedableRng, StdRng};

mod instruction;
mod matrices;

pub use crate::instruction::{Gate, Instruction, Kind, Program};
use crate::matrices::{I, Matrix, SWAP, Vector, kronecker_expt, lift};

/// A quantum interpreter.
pub struct Machine {
    num_qubits: u32,
    state: Vector,
    register: u32,
    rng: StdRng,
}

impl Machine {
    /// Create a new machine
    #[must_use]
    pub fn new(num_qubits: u32, seed: u64) -> Self {
        let mut state = Vector::zeros(2usize.pow(num_qubits));
        state[0] = 1.0.into();
        Self {
            num_qubits,
            state,
            register: 0,
            rng: StdRng::seed_from_u64(seed),
        }
    }

    /// Run the given `Program` on this `Machine`.
    ///
    /// # Errors
    ///
    /// If any instruction in the program addresses a qubit bigger than the number of qubits in
    /// this machine, this will error.
    pub fn run(&mut self, program: &Program) -> Result<u32, String> {
        for i in &program.instructions {
            match i {
                Instruction::Gate(Gate::OneQ { kind, qubit }) => {
                    self.apply_1q(&kind.to_matrix(), *qubit)?;
                }
                Instruction::Gate(Gate::TwoQ { kind, qubits }) => {
                    self.apply_2q(&kind.to_matrix(), *qubits)?;
                }
                Instruction::Measure => {
                    self.register = self.sample();
                    self.state.fill(0.0.into());
                    self.state[0] = 1.0.into();
                }
            }
        }
        Ok(self.register)
    }

    fn apply_1q(&mut self, u: &Matrix, q: u32) -> Result<(), String> {
        if q >= self.num_qubits {
            Err(format!("This machine only has {} qubits.", self.num_qubits))
        } else {
            self.state = lift(u, q, self.num_qubits) * &self.state;
            Ok(())
        }
    }

    fn apply_2q(&mut self, u: &Matrix, qs: (u32, u32)) -> Result<(), String> {
        let (q1, q2) = qs;
        if q1 >= self.num_qubits || q2 >= self.num_qubits {
            Err(format!("This machine only has {} qubits.", self.num_qubits))
        } else {
            let trans_to_op = |t: &[u32]| {
                t.iter()
                    .fold(kronecker_expt(&I, self.num_qubits), |acc, i| {
                        acc * lift(&SWAP, *i, self.num_qubits)
                    })
            };
            let from_space = [q2, q1]
                .into_iter()
                .chain((0..self.num_qubits).filter(|&i| q1 != i && q2 != i))
                .collect::<Vec<_>>();
            let trans = transpositions_to_adjacents(&permutation_to_transpositions(&from_space));
            let to_from = trans_to_op(&trans);
            let from_to = trans_to_op(&trans.into_iter().rev().collect::<Vec<_>>());
            let u = to_from * lift(u, 0, self.num_qubits) * from_to;
            self.state = u * &self.state;
            Ok(())
        }
    }

    fn sample(&mut self) -> u32 {
        let mut r = self.rng.r#gen::<f64>();
        for (i, psi) in self.state.iter().enumerate() {
            r -= psi.norm_sqr();
            if r < 0.0 {
                return i.try_into().unwrap();
            }
        }
        self.num_qubits - 1
    }
}

fn permutation_to_transpositions(p: &[u32]) -> Vec<(u32, u32)> {
    p.iter()
        .enumerate()
        .filter_map(|(mut src, &dest)| {
            let d = dest as usize;
            while src < d {
                src = p[src] as usize;
            }
            (src > d).then_some((dest, u32::try_from(src).unwrap()))
        })
        .collect()
}

fn transpositions_to_adjacents(t: &[(u32, u32)]) -> Vec<u32> {
    t.iter()
        .flat_map(|&(a, b)| {
            if b.saturating_sub(a) <= 1 {
                vec![a]
            } else {
                (a..b).chain((a..b - 1).rev()).collect()
            }
        })
        .collect()
}
