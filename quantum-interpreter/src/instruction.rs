use crate::matrices::{CNOT, H, I, Matrix, SWAP};
use faer::c64;
use std::{str::FromStr, sync::Arc};

/// A quantum program to interpret
pub struct Program {
    /// A program consists of a collection of instructions
    pub instructions: Vec<Instruction>,
}

impl FromStr for Program {
    type Err = String;
    fn from_str(input: &str) -> Result<Self, Self::Err> {
        parser::program(input.trim()).map_err(|e| e.to_string())
    }
}

/// A quantum instruction
pub enum Instruction {
    /// A gate instruction
    Gate(Gate),
    /// Measure all qubits
    Measure,
}

/// A gate instruction
pub enum Gate {
    /// A one-qubit gate instruction
    OneQ {
        /// The kind of gate involved
        kind: Kind,
        /// The qubit to which this gate applies
        qubit: u32,
    },
    /// A two-qubit gate instruction
    TwoQ {
        /// The kind of gate involved
        kind: Kind,
        /// The qubit to which this gate applies
        qubits: (u32, u32),
    },
}

impl FromStr for Instruction {
    type Err = String;
    fn from_str(input: &str) -> Result<Self, Self::Err> {
        parser::instruction(input.trim()).map_err(|e| e.to_string())
    }
}

/// The gates supported by our quantum interpreter
pub enum Kind {
    /// The I gate
    I,
    /// The SWAP gate
    Swap,
    /// The Hadamard gate
    H,
    /// The CNOT gate
    CNot,
    /// The CPHASE gate, which takes an angle argument
    Cphase(f64),
}

impl Kind {
    pub(crate) fn to_matrix(&self) -> Arc<Matrix> {
        match self {
            Self::I => I.clone(),
            Self::Swap => SWAP.clone(),
            Self::H => H.clone(),
            Self::CNot => CNOT.clone(),
            Self::Cphase(angle) => {
                let mut u = Matrix::identity(4, 4);
                u[(3, 3)] = c64::cis(*angle);
                Arc::new(u)
            }
        }
    }
}

peg::parser! {
    grammar parser() for str {
        pub(super) rule program() -> Program =
            instructions:(instruction() ** "\n") { Program { instructions } }
        pub(super) rule instruction() -> Instruction = measure() / gate()
        rule measure() -> Instruction = "MEASURE" { Instruction::Measure }
        rule gate() -> Instruction = i() / swap() / h() / cnot() / cphase()
        rule i() -> Instruction =
            "I" space() qubit:one_qubit()
            { Instruction::Gate(Gate::OneQ { kind: Kind::I, qubit }) }
        rule swap() -> Instruction =
            "SWAP" space() qubits:two_qubits()
            { Instruction::Gate(Gate::TwoQ { kind: Kind::Swap, qubits }) }
        rule h() -> Instruction =
            "H" space() qubit:one_qubit()
            { Instruction::Gate(Gate::OneQ { kind: Kind::H, qubit }) }
        rule cnot() -> Instruction =
            "CNOT" space() qubits:two_qubits()
            { Instruction::Gate(Gate::TwoQ { kind: Kind::CNot, qubits }) }
        rule cphase() -> Instruction =
            "CPHASE(" angle:float() ")" space() qubit:one_qubit()
            { Instruction::Gate(Gate::OneQ{ kind: Kind::Cphase(angle), qubit }) }
        rule one_qubit() -> u32 = q:int() { q }
        rule two_qubits() -> (u32, u32) = p:int() space() q:int() { (p, q) }
        rule int() -> u32 = i:$(['0'..='9']+) {? i.parse::<u32>().or(Err("int")) }
        rule float() -> f64 =
            f:$((['-']?['0'..='9']+)("." ['0'..='9']+)?)
            {? f.parse::<f64>().or(Err("float")) }
        rule space() = quiet!{[' ' | '\t']+}
    }
}
