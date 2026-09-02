"""Simulating 2D transverse-field Ising model from _Evidence for the utility of
quantum computing before fault tolerance_[^1].

For the θ_h values, see [^2], specifically the first column of
data/fig4b_experiment_mit.txt.

[^1]: https://www.nature.com/articles/s41586-023-06096-3
[^2]: https://github.com/youngseok-kim1/Evidence-for-the-utility-of-quantum-computing-before-fault-tolerance
"""

from enum import StrEnum, unique

import numpy as np
from bluequbit import BQClient
from bluequbit.library.helpers.hardware_connectivites import IBM_127_HEAVY_HEX_MAP
from pydantic import BaseModel
from qiskit import QuantumCircuit

N_QUBIT = 127  # 127 qubits on IBM heavy hex
THETA_J = -np.pi / 2  # Fixed value in paper
THETA_HS = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.0, 1.5707]


@unique
class Observable(StrEnum):
    """Observables of which we take the expectation running the simulated
    Trotterized circuit.

    `M` is the magnetization observable; `Z` is the observable <Z_62>.
    """

    M = "M"
    Z = "Z"

    def pauli(self) -> str | list[str]:
        """Pauli string(s) for this observable.

        Returns:
            A Pauli string or list of Pauli strings.
        """
        return {
            Observable.M: magnetization_paulis(N_QUBIT),
            Observable.Z: "I" * 62 + "Z" + "I" * (127 - 62 - 1),
        }[self]


@unique
class PPSDevice(StrEnum):
    """BlueQubit Pauli Path Simulation device."""

    CPU = "pauli-path"
    GPU = "pauli-path.gpu"


def magnetization_paulis(n_qubit: int = N_QUBIT) -> list[str]:
    """M_Z magnetization observable.

    Args:
        n_qubit: Number of qubits; default = 127.

    Raises:
        `ValueError` if `n_qubit` < 1.

    Returns:
        A Pauli string or list of Pauli strings.

    Examples:
    >>> magnetization_paulis(5)
    ['ZIIII', 'IZIII', 'IIZII', 'IIIZI', 'IIIIZ']
    """
    if n_qubit < 1:
        raise ValueError("n_qubit must be ≥ 1.")
    return ["I" * i + "Z" + "I" * (n_qubit - i - 1) for i in range(n_qubit)]


def trotterized_circuit(
    theta_h: float,
    n_trotter: int,
    *,
    n_qubit: int = N_QUBIT,
    theta_j: float = THETA_J,
    lattice: list[list[int]] = IBM_127_HEAVY_HEX_MAP,
) -> QuantumCircuit:
    """Build the Trotterized circuit from the paper.

    Args:
        theta_h: R_X angle.
        n_trotter: Number of Trotter steps.
        n_qubit: Number of qubits; default = 127.
        theta_j: R_ZZ angle; default = -π/2.
        lattice: 2Q connectivity map; default = IBM 127-qubit heavy hex.

    Raises:
        `ValueError` if any of the arguments are out of range.

    Returns:
        The quantum circuit.

    Examples:
    >>> trotterized_circuit(0, 1, n_qubit=2, lattice=[[0, 1]]).draw()
                    ┌───────┐
    q_0: ─■─────────┤ Rx(0) ├
          │ZZ(-π/2) ├───────┤
    q_1: ─■─────────┤ Rx(0) ├
                    └───────┘
    """
    if not 0 <= theta_h <= np.pi / 2:
        raise ValueError("theta_h must be in [0, π/2].")
    elif not -np.pi <= theta_j <= np.pi:
        raise ValueError("theta_j must be in [-π, π].")
    elif n_qubit < 1:
        raise ValueError("n_qubit must be ≥ 1.")
    elif n_trotter < 1:
        raise ValueError("n_trotter must be ≥ 1.")
    qc = QuantumCircuit(n_qubit)
    for _ in range(n_trotter):
        for edge in lattice:
            qc.rzz(theta_j, *edge)
        for q in range(n_qubit):
            qc.rx(theta_h, q)
    return qc


class Parameters(BaseModel):
    """Parameters to submit multiple circuits to BlueQubit at once.

    Args:
        n_trotter: Number of Trotter steps.
        device: BlueQubit Pauli Path Simulation device.
        delta: Lower bound on absolute value of coefficients of the Pauli
            operators retained after each step of the Pauli-path propagation
            algorithm; passed as `pauli_path_truncation_threshold` in
            `bq.run`'s `options` dictionary.
        level: Extent of transpilation performed; passed as
            `pauli_path_circuit_transpilation_level` in `bq.run`'s `options`
            dictionary.
    """

    n_trotter: int
    observable: Observable
    device: PPSDevice
    timeout: float | None
    delta: float
    level: int | None

    def __post_init__(self):
        if self.n_trotter < 1:
            raise ValueError("n_trotter must be ≥ 1.")
        elif not 1e-5 <= self.delta <= 1:
            raise ValueError("delta must be in [1e-5, 1].")
        elif self.level is not None and self.level not in [1, 2, 3]:
            raise ValueError("level must be one of 1, 2, 3, or None.")

    @property
    def name(self) -> str:
        return "_".join(str(f) for f in self.model_dump().values())

    def circuits(self, theta_hs: list[float]) -> list[QuantumCircuit]:
        """Construct the Trotterized circuits for this input batch."""
        return [trotterized_circuit(t, self.n_trotter) for t in theta_hs]


class Result(BaseModel):
    """A single experiment's output from BlueQubit.

    Args:
        job_id: Job ID from BlueQubit.
        batch_id: Batch ID from BlueQubit.
        theta_h: R_X angle.
        parameters: The experiment's input parameters.
        run_time_ms: Runtime.
        cost: Cost in US dollars.
        expectation: Expectation value.
    """

    job_id: str
    batch_id: str | None
    theta_h: float
    parameters: Parameters
    run_time_ms: int | None
    cost: float
    expectation: float

    def flatten(
        self,
    ) -> dict[str, str | float | int | Observable | PPSDevice | None]:
        """Flatten out this result, e.g. to store in a dataframe."""
        out = self.model_dump(exclude={"parameters"})
        out.update(self.parameters.model_dump())
        return out


def run(
    p: Parameters, bq: BQClient, theta_hs: list[float] | None = None
) -> list[Result]:
    """Run a set of experiments for each θ_h, return the results.

    If `theta_hs` is `None`, use all the θ_h values from the paper.
    Raises a `ValueError` if given improper θ_h values.
    """
    if theta_hs is None:
        theta_hs = THETA_HS
    if any(t not in THETA_HS for t in theta_hs):
        raise ValueError("Only running on subsets of the paper's θ_h values.")
    out = bq.run(
        p.circuits(theta_hs),
        device=str(p.device),
        pauli_sum=p.observable.pauli(),  # type: ignore (variance issue)
        job_name=p.name,
        timeout=p.timeout,
        options={
            "pauli_path_truncation_threshold": p.delta,
            "pauli_path_circuit_transpilation_level": p.level,
        },
    )
    if not isinstance(out, list):
        out = [out]
    return [
        Result(
            job_id=r.job_id,
            batch_id=r.batch_id,
            theta_h=t,
            parameters=p,
            run_time_ms=r.run_time_ms,
            cost=r.cost,
            expectation=np.average(r.expectation_value).item(),
        )
        for t, r in zip(theta_hs, out)
    ]
