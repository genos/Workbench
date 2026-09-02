import marimo

__generated_with = "0.24.0"
app = marimo.App(width="medium")


@app.cell
def _():
    import driver
    import numpy as np
    import matplotlib.pyplot as plt
    import pandas as pd
    import seaborn as sns

    return driver, pd, sns


@app.cell
def _(sns):
    sns.set_theme(
        context="talk",
        font="Equity A",
        style="white",
        rc={
            "axes.spines.top": False,
            "axes.spines.right": False,
        },
    )
    return


@app.cell
def _(driver):
    df = driver.load()
    df
    return (df,)


@app.cell
def _(df, pd, sns):
    _estimated = df.loc[
        df.observable == "M",
        ["theta_h", "run_time_ms", "cost", "expectation", "n_trotter"]
    ].reset_index(drop=True)
    _exact = pd.read_csv("data/fig3a_exact.csv")
    _t5 = pd.merge(
        _estimated,
        _exact,
        how="left",
        on="theta_h",
        suffixes=("_estimated", "_exact"),
    )
    _m = _t5.melt(
        id_vars=["theta_h"],
        value_vars=["expectation_estimated", "expectation_exact"],
        var_name="Method",
        value_name=r"$M_Z$",
    )
    _m.Method = _m.Method.str.removeprefix("expectation_")
    _m
    _m[r"$\theta_h$"] = _m.theta_h
    g_t5 = sns.lineplot(
        _m,
        x=r"$\theta_h$",
        y=r"$M_Z$",
        hue="Method",
        marker="o",
        palette="Paired"
    )
    g_t5.figure.suptitle("Comparison of PPS Estimate and Exact")
    g_t5.set_title("Magnetization Observable at 5 Trotter Steps")
    g_t5.figure.tight_layout()
    g_t5
    return


@app.cell
def _(df, pd, sns):
    _gpu = df.loc[
        (df.observable == "Z")
        & (df.n_trotter == 20)
        & df.delta.isin([1e-2, 1e-3, 1e-4]),
        ["theta_h", "run_time_ms", "expectation", "delta"],
    ].reset_index(drop=True)
    _qpu = pd.read_csv("data/fig4b_mit.csv")
    _20 = pd.merge(
        _gpu, _qpu, how="left", on="theta_h", suffixes=("_GPU", "_QPU")
    )
    _m = _20.melt(
        id_vars=["theta_h", "delta"],
        value_vars=["expectation_GPU", "expectation_QPU"],
        var_name="Hardware",
        value_name=r"$\mathcal{O}_k\approx\langle Z_{62}\rangle$",
    )
    _m.Hardware = _m.Hardware.str.removeprefix("expectation_")
    _m[r"$\delta$"] = _m.delta
    _m[r"$\theta_h$"] = _m.theta_h
    g_20 = sns.relplot(
        _m,
        x=r"$\theta_h$",
        y=r"$\mathcal{O}_k\approx\langle Z_{62}\rangle$",
        hue="Hardware",
        marker="o",
        kind="line",
        col=r"$\delta$",
        col_order=[1e-2, 1e-3, 1e-4],
        height=4.5,
        palette="Paired"
    )
    g_20.figure.suptitle(r"GPU vs. QPU at 20 Trotter Steps")
    g_20.tight_layout()
    g_20
    return


@app.cell
def _(df, pd):
    large = df.loc[
        (df.observable == "Z") & df.delta.isin([1e-2, 1e-3, 1e-4]),
        ["theta_h", "n_trotter", "run_time_ms", "expectation", "delta"],
    ].reset_index(drop=True)
    large[r"$\theta_h$"] = pd.Categorical(large.theta_h)
    large["$T$"] = large.n_trotter
    large["Runtime (ms)"] = large.run_time_ms
    large[r"$\delta$"] = large.delta
    large[r"$\mathcal{O}_k\approx\langle Z_{62}\rangle$"] = large.expectation
    return (large,)


@app.cell
def _(large, sns):
    g_t = sns.relplot(
        large,
        x=r"$\theta_h$",
        y="Runtime (ms)",
        hue="$T$",
        col=r"$\delta$",
        col_order=[1e-2, 1e-3, 1e-4],
        kind="line",
        palette="rocket_r",
    )
    for _ax in g_t.axes.flat:
        _ax.set_yscale("log")
    g_t
    return


@app.cell
def _(large, sns):
    g_e = sns.relplot(
        large, 
        x=r"$\delta$",
        y=r"$\mathcal{O}_k\approx\langle Z_{62}\rangle$",
        hue=r"$\theta_h$",
        kind="line",
        col="$T$",
        col_wrap=4,
        palette="rocket_r",
    )
    for _ax in g_e.axes.flat:
        _ax.invert_xaxis()
        _ax.set_xscale("log")
    g_e
    return


if __name__ == "__main__":
    app.run()
