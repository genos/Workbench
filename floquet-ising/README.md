# floquet-ising

Simulating 2D transverse-field Ising model from _Evidence for the utility of
quantum computing before fault tolerance_[^1].

# Introduction

Some ideas and code were borrowed & modified from the BlueQubit tutorial[^2].
Similarly, we also consulted _Fast and converged classical simulations of
evidence for the utility of quantum computing before fault tolerance_[^3] and
_A Practical Guide to using Pauli Path Simulators for Utility-Scale Quantum
Experiments_[^4].
The $`\theta_h`$ values for the $`R_X`$ gates were gleaned from the code & data
supplement to the original paper[^5], specifically the data used for the
"exact" values in Figure 3a; however, rather than the 156 different
$`\theta_h`$ values they used for the exact solution (see
`data/fig3a_exact.txt`), we opt for the 11 from their experiments
(`data/fig4b_experiment_mit.txt`).

# Project Organization

This project is managed with [`uv`](https://docs.astral.sh/uv/).

- Reusable code is in the `floquet-ising` package, with code in `src/` and the
  accompanying `pyproject.toml`. Some of the code's docstrings contain examples
  that can be tested via `uv run pytest --doctest-modules`.
- Driving functions & utilities are in `driver.py`.
- Analysis & visualizations are in `analysis.py`.
- In `data/`, you'll find `fig3a_exact.csv` and `fig4b_mit.csv`, _very_
  slightly processed versions of data from [^5] for exact values of $`M_Z`$
  with 5 Trotter steps and QPU values of $`\langle Z_{62}\rangle`$ with 20,
  respectively.
- Also in `data/`, the file `results.parquet.zst` is a data frame storing our
  experimental results, in the Parquet format & compressed with `zstd`.
- Finally, `writeup/` contains PNG plots from `analysis.py` and the markdown
  source of the take-home write-up.


# References

[^1]: https://www.nature.com/articles/s41586-023-06096-3
[^2]: https://app.bluequbit.io/tutorial/pauli-path-simulation
[^3]: https://arxiv.org/abs/2308.05077
[^4]: https://arxiv.org/abs/2507.10771
[^5]: https://github.com/youngseok-kim1/Evidence-for-the-utility-of-quantum-computing-before-fault-tolerance
