---
title: BlueQubit Take-Home Task
author: Graham Enos
bibliography: ref.bib
...

For this take-home task, we were asked to explore classically simulating the
kicked Ising model from the IBM paper _Evidence for the utility of quantum
computing before fault tolerance_.\footnote{Kim et al. (2023)}
Specifically, we were asked:

> What is the largest number of Trotter steps $T$ of this dynamics on 127 qubits
> with IBM heavy-hex layout that you can produce classically, defensible to
> within $2\%$ (i.e. absolute error of $\sim 0.02$ on the $[0,1]$ range), across the
> range of $R_X$ angles presented in the paper?

\noindent The largest value of $T$ we were able to produce is **20 Trotter
steps**.
We'll go into more details about our experimentation and analysis, but first:
more on the model itself.

# Simulating the Kicked Ising Model

The model from @IBM2023 is a Floquet version of the Ising model.
Each of the $T$ time steps consists of $R_{ZZ}$ gates with fixed angle
$\theta_j = \frac{-\pi}{2}$ followed by $R_X$ gates of variable $\theta_h$
which impart "kicks."
For example, here's a circuit diagram on a linear chain of four qubits with $T
= 2$ and $\theta_h = 0$:

\includegraphics{circuit.png}

\noindent This circuit is simple enough to construct in `qiskit`; here's a snippet of the
code we used, modified from BlueQubit's _very_ helpful tutorial:\footnote{BlueQubit (2026)}

```python
qc = QuantumCircuit(n_qubit)
for _ in range(n_trotter):
    for edge in lattice:
        qc.rzz(theta_j, *edge)
    for q in range(n_qubit):
        qc.rx(theta_h, q)
return qc
```

With the help of the @BQ2026 tutorial and the _Practical Guide_ paper---@Practical2025,
which cites and extends @Fast2024---we were able to run classical simulations
of the model on the BlueQubit platform via Pauli Path Simulation on a
GPU.\footnote{We ran some smaller PPS examples on a CPU to test out the code
and data gathering workflow, but ultimately, the performance boosts from the
GPU version (obtained by passing \texttt{device="pauli-path.gpu"} ) were
absolutely necessary for larger circuits.}
These experiments used IBM's full 127-qubit heavy hexagon lattice.

# Matching IBM at 5 Trotter steps

We next endeavored to match the IBM experiment with $T = 5$, at which depth
they were able to classically verify the expected values.
Taking the eleven $\theta_h$ values @IBM2023 used,\footnote{See
\texttt{data/fig4b\_experiment\_mit.txt} in their GitHub repo.} we estimated
the magnetization $M_Z$.\footnote{Compare the following with Fig. 3a in Kim et
al. (2023).}
For many of these $\theta_h$, a truncation parameter of $\delta = 10^{-2}$
sufficed to match the exact values within the desired precision; all matched by
$\delta = 10^{-3}$.

\includegraphics[width=0.5\textwidth]{m_5.png}

# 20 Trotter steps

After working on $T = 5$, we moved on to Figure 4b from @IBM2023.
This had us estimating $\langle Z_{62}\rangle$ after $T = 20$ Trotter steps.
The performance of our estimate $\mathcal{O}_k$ of the observable fared
differently for different values of $\theta_h$ and $\delta$, largely matching
the behavior seen in @Practical2025.

\includegraphics{z_20.png}

As you can see above, the more Clifford/less "magic" values of $\theta_h$
nearer to $0$ and $\frac{\pi}{2}$ converged to IBM's QPU values for nearly all
of the $\delta$ truncations we tried.
The maximally non-Clifford $\theta_h$, specifically $0.6$ and $0.7$, were more
difficult to get close to the QPU values.
Interestingly, as @Practical2025 notes, smaller values of $\delta$ didn't
necessarily improve convergence.
The above graph only shows $\delta \in \lbrace 10^{-2}, 10^{-3}, 10^{-4}
\rbrace$, but our experiments explored $\delta$ much closer to the BlueQubit
platform's cutoff of $10^{-5}$; of all of these, $\mathcal{O}_k$ was best at
$\delta = 10^{-3}$.

# Varying $T$, examining convergence with respect to $\delta$

The platform wouldn't allow experiments with $T$ greater than 20 or $\delta$
less than $10^{-5}$, so we have to claim $T = 20$ as the largest we could
classically simulate.
However, we also investigated the behavior of the model over a larger
collection of $T$ values.

\includegraphics{runtime.png}

Experimenting with even values of $T = 8, 10, ..., 20$ and the eleven
$\theta_h$ values previously used, a few things stand out with respect to
runtimes.
First, there are some outliers, runtimes longer than expected compared to
neighboring times.
These can largely be attributed to time spent at the start of an experimental
run; things ran much faster later in a run after the GPU, client
connection, platform, and workflow had warmed up.
Second, for any $T$, the runtime for $\theta_h = 0$ is---except for the
aforementioned outliers---always the shortest.
The similarly Clifford $\theta = \frac{pi}{2}$ is often quick as well,
especially for smaller $T$.
Finally, the runtime grows steadily with $T$, except for some noisier times
around the challenging $\theta_h$ values at higher $T$.

\includegraphics{convergence.png}

Looking for convergence of the estimate $\mathcal{O}_k \approx \langle Z_{62}
\rangle$ at $T = 8, 10, ..., 20$ for the eleven $\theta_h$ values, our
conclusions again seem to agree with @Practical2025.
For $\theta_h$ nearer to $0$ or $\frac{\pi}{2}$, $\mathcal{O}_k$ seems to
converge for even larger values of $\delta$, even for large Trotter steps $T$. 
The more troublesome $\theta_h$---$0.6$, $0.7$, and $0.8$---don't obviously
converge with smaller $\delta$, regardless of the number of Trotter steps.
Luckily for those working on this take-home task, the platform's GPU
implementation of PPS is performant enough to obtain results similar to those
in @Practical2025 in a fraction of the time.

# Conclusion

As stated above, the maximum we were able to simulate is **20 Trotter steps**.
The references below were vital to this work, as was the BlueQubit platform
itself.
Having such a high-quality GPU implementation of Pauli Path Simulation made
exploration quick and easy.
What's more, all of this experimentation cost less than half of our granted
\$150 platform credits!

# References
