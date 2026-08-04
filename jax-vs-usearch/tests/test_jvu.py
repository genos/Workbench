import numpy as np
import pytest
from jax_vs_usearch import via_jax as vj, via_usearch as vu


def _go(method, inputs, outputs, queries):
    lookup = method(inputs, outputs)
    return lookup.search(queries, 5)


@pytest.mark.benchmark
@pytest.mark.parametrize(
    "method", [vj.L2Similarity, vu.L2Similarity], ids=["jax", "usearch"]
)
def test_benchmark(benchmark, method):
    rng = np.random.default_rng(49953282)  # random.org
    inputs = rng.uniform(size=(1_000, 1_000)).astype(np.float32)
    outputs = rng.uniform(size=(1_000, 500)).astype(np.float32)
    queries = rng.uniform(size=(500, 1_000)).astype(np.float32)
    result = benchmark(_go, method, inputs, outputs, queries)
    assert result.shape == (500, 500)
