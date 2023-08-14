use rand::{rngs::StdRng, Rng, SeedableRng};

mod instruction;
mod matrices;

pub use instruction::{Gate, Instruction, Program};
use matrices::{kronecker_expt, lift, Matrix, Vector, I, SWAP};

pub struct Machine {
    num_qubits: usize,
    state: Vector,
    register: usize,
    rng: StdRng,
}

impl Machine {
    #[must_use]
    pub fn new(num_qubits: usize, seed: u64) -> Self {
        let mut state =
            Vector::zeros(2usize.pow(u32::try_from(num_qubits).expect("Should be small enough")));
        state[0] = 1.0.into();
        Self {
            num_qubits,
            state,
            register: 0,
            rng: StdRng::seed_from_u64(seed),
        }
    }

    pub fn run(&mut self, program: &Program) -> usize {
        program.instructions.iter().for_each(|i| match i {
            Instruction::Gate { gate, qubits } => self.apply(&gate.to_matrix(), qubits),
            Instruction::Measure => {
                let b = self.sample();
                self.state.fill(0.0.into());
                self.state[0] = 1.0.into();
                self.register = b;
            }
        });
        self.register
    }

    fn apply(&mut self, u: &Matrix, qs: &[usize]) {
        assert!(
            qs.iter().all(|&q| q < self.num_qubits),
            "This machine only has {} qubits.",
            self.num_qubits
        );
        if let [q] = qs {
            self.state = lift(u, *q, self.num_qubits).dot(&self.state);
        } else {
            let trans_to_op = |t: &[usize]| {
                t.iter()
                    .fold(kronecker_expt(&I, self.num_qubits), |acc, i| {
                        acc.dot(&lift(&SWAP, *i, self.num_qubits))
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
            let u = to_from.dot(&lift(u, 0, self.num_qubits).dot(&from_to));
            self.state = u.dot(&self.state);
        }
    }

    fn sample(&mut self) -> usize {
        let mut r = self.rng.gen::<f64>();
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
