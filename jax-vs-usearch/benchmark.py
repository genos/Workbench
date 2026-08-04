import json
import shlex
import subprocess
from pathlib import Path
from tempfile import NamedTemporaryFile
from typing import Annotated

import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
import typer


def main(
    n: Annotated[
        int, typer.Option(help="Min # of rounds for benchmarking", min=1)
    ] = 20,
    png: Annotated[Path, typer.Option(help="PNG name for plot")] = Path(__file__).parent
    / "benchmark.png",
):
    with NamedTemporaryFile(suffix=".json") as t:
        subprocess.check_call(
            shlex.split(
                f"uv run pytest -q --benchmark-min-rounds={n} --benchmark-json={t.name}"
            )
        )
        with open(t.name) as f:
            df = pd.DataFrame(
                {
                    "Version": b["name"]
                    .removeprefix("test_benchmark[")
                    .removesuffix("]"),
                    "Time (s)": t,
                }
                for b in json.load(f)["benchmarks"]
                for t in b["stats"]["data"]
            )
    sns.set_theme()
    sns.stripplot(df, x="Version", y="Time (s)", hue="Version", alpha=0.75)
    plt.savefig(str(png), dpi=300)


if __name__ == "__main__":
    typer.run(main)
