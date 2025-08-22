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

pub use crate::instruction::{Gate, Instruction, Program};
use crate::matrices::{I, Matrix, SWAP, Vector, kronecker_expt, lift};

/// A quantum interpreter.
pub struct Machine {
    num_qubits: usize,
    state: Vector,
    register: usize,
    rng: StdRng,
}

impl Machine {
    /// Create a new machine
    ///
    /// # Errors
    ///
    /// If `num_qubits` is bigger than can fit into a `u32`, this will error.
    pub fn new(num_qubits: usize, seed: u64) -> Result<Self, String> {
        let mut state = Vector::zeros(
            2usize.pow(u32::try_from(num_qubits).map_err(|_| "num_qubits should fit into a u32")?),
        );
        state[0] = 1.0.into();
        Ok(Self {
            num_qubits,
            state,
            register: 0,
            rng: StdRng::seed_from_u64(seed),
        })
    }

    /// Run the given `Program` on this `Machine`.
    ///
    /// # Errors
    ///
    /// If any instruction in the program addresses a qubit bigger than the number of qubits in
    /// this machine, this will error.
    pub fn run(&mut self, program: &Program) -> Result<usize, String> {
        for i in &program.instructions {
            match i {
                Instruction::Gate { gate, qubits } => self.apply(&gate.to_matrix(), qubits)?,
                Instruction::Measure => {
                    let b = self.sample();
                    self.state.fill(0.0.into());
                    self.state[0] = 1.0.into();
                    self.register = b;
                }
            }
        }
        Ok(self.register)
    }

    fn apply(&mut self, u: &Matrix, qs: &[usize]) -> Result<(), String> {
        if !qs.iter().all(|&q| q < self.num_qubits) {
            return Err(format!("This machine only has {} qubits.", self.num_qubits));
        }
        if let [q] = qs {
            self.state = lift(u, *q, self.num_qubits) * &self.state;
        } else {
            let trans_to_op = |t: &[usize]| {
                t.iter()
                    .fold(kronecker_expt(&I, self.num_qubits), |acc, i| {
                        acc * lift(&SWAP, *i, self.num_qubits)
                    })
            };
            let from_space = qs
                .iter()
                .copied()
                .rev()
                .chain((0..self.num_qubits).filter(|i| !qs.contains(i)))
                .collect::<Vec<_>>();
            let trans = transpositions_to_adjacents(&permutation_to_transpositions(&from_space));
            let to_from = trans_to_op(&trans);
            let from_to = trans_to_op(&trans.into_iter().rev().collect::<Vec<_>>());
            let u = to_from * lift(u, 0, self.num_qubits) * from_to;
            self.state = u * &self.state;
        }
        Ok(())
    }

    fn sample(&mut self) -> usize {
        let mut r = self.rng.r#gen::<f64>();
        for (i, psi) in self.state.iter().enumerate() {
            r -= psi.norm_sqr();
            if r < 0.0 {
                return i;
            }
        }
        self.num_qubits - 1
    }
}

fn permutation_to_transpositions(p: &[usize]) -> Vec<(usize, usize)> {
    p.iter()
        .enumerate()
        .filter_map(|(mut src, &dest)| {
            while src < dest {
                src = p[src];
            }
            (src > dest).then_some((dest, src))
        })
        .collect()
}

fn transpositions_to_adjacents(t: &[(usize, usize)]) -> Vec<usize> {
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
