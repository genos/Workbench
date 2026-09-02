"""Driver functions & utilities for gathering data."""

import bluequbit
import pandas as pd
from pyprojroot import here

import floquet_ising as fi

RESULTS = here() / "data" / "results.parquet.zst"


def load() -> pd.DataFrame:
    return pd.read_parquet(RESULTS)


def dump(df: pd.DataFrame):
    df.to_parquet(RESULTS, compression="zstd")


def save(xs: list[fi.Result]):
    dump(
        pd.concat([load(), pd.DataFrame([x.flatten() for x in xs])], ignore_index=True)
    )


def run(
    *,
    n_trotter: int,
    observable: fi.Observable | str,
    delta: float,
    timeout: float | None = 5 * 60,
    level: int | None = None,
    device: fi.PPSDevice | str = fi.PPSDevice.GPU,
    theta_hs: list[float] | None = None,
    bq: bluequbit.BQClient | None = None,
) -> list[fi.Result]:
    return fi.run(
        fi.Parameters(
            n_trotter=n_trotter,
            observable=fi.Observable(observable),
            device=fi.PPSDevice(device),
            delta=delta,
            timeout=timeout,
            level=level,
        ),
        bq=bq if bq is not None else bluequbit.init(),
        theta_hs=theta_hs,
    )
