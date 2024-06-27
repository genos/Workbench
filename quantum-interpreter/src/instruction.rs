use crate::matrices::{Matrix, CNOT, H, I, SWAP};
use num_complex::Complex64;
use std::str::FromStr;
use std::sync::Arc;

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
    Gate {
        /// The kind of gate involved
        gate: Gate,
        /// The qubits to which this gate applies
        qubits: Vec<usize>,
    },
    /// Measure all qubits
    Measure,
}

impl FromStr for Instruction {
    type Err = String;
    fn from_str(input: &str) -> Result<Self, Self::Err> {
        parser::instruction(input.trim()).map_err(|e| e.to_string())
    }
}

/// The gates supported by our quantum interpreter
pub enum Gate {
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

impl Gate {
    pub(crate) fn to_matrix(&self) -> Arc<Matrix> {
        match self {
            Self::I => I.clone(),
            Self::Swap => SWAP.clone(),
            Self::H => H.clone(),
            Self::CNot => CNOT.clone(),
            Self::Cphase(angle) => {
                let mut u = Matrix::eye(4);
                u[[3, 3]] = Complex64::cis(*angle);
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
            "I" space() qubits:one_qubit()
            { Instruction::Gate { gate: Gate::I, qubits } }
        rule swap() -> Instruction =
            "SWAP" space() qubits:two_qubits()
            { Instruction::Gate { gate: Gate::Swap, qubits } }
        rule h() -> Instruction =
            "H" space() qubits:one_qubit()
            { Instruction::Gate { gate: Gate::H, qubits } }
        rule cnot() -> Instruction =
            "CNOT" space() qubits:two_qubits()
            { Instruction::Gate { gate: Gate::CNot, qubits } }
        rule cphase() -> Instruction =
            "CPHASE(" angle:float() ")" space() qubits:one_qubit()
            { Instruction::Gate { gate: Gate::Cphase(angle), qubits } }
        rule one_qubit() -> Vec<usize> = q:int() { vec![q] }
        rule two_qubits() -> Vec<usize> = p:int() space() q:int() { vec![p, q] }
        rule int() -> usize = i:$(['0'..='9']+) {? i.parse::<usize>().or(Err("int")) }
        rule float() -> f64 =
            f:$((['-']?['0'..='9']+)("." ['0'..='9']+)?)
            {? f.parse::<f64>().or(Err("float")) }
        rule space() = quiet!{[' ' | '\t']+}
    }
}
