"""Similarity search on numpy arrays via usearch."""

import numpy as np
from numpy.typing import NDArray
from usearch.index import Index


class L2Similarity:
    """Similarity search on numpy arrays using L2 distance via usearch.

    Args:
        inputs: Input vectors through which to search.
        outputs: Output labels corresponding to the input vectors.

    Examples:

    >>> l2 = L2Similarity(
    ...    np.array([[0, 0, 0], [0.25, 0.25, 0.25], [1, 1, 1]], dtype=np.float32),
    ...    np.array([[0, 0], [0.25, 0.25], [1, 1]], dtype=np.float32),
    ... )
    >>> l2.search(np.array([[0, 0, 0], [1, 1, 0]], dtype=np.float32), k=1)
    array([[0., 0.],
           [1., 1.]], dtype=float32)
    """

    def __init__(self, inputs: NDArray[np.float32], outputs: NDArray[np.float32]):
        if inputs.shape[0] != outputs.shape[0]:
            raise ValueError("Input vectors and output vectors differ in 0th dimension")
        if inputs.dtype != np.float32 or outputs.dtype != np.float32:
            raise ValueError("Input and output must have dtype np.float32.")
        self.index = Index(ndim=inputs.shape[1], metric="l2sq", dtype="f32")
        self.index.add(np.arange(inputs.shape[0]), inputs)
        self.outputs = outputs

    def search(self, xs: NDArray[np.float32], k: int = 5) -> NDArray[np.float32]:
        matches = self.index.search(xs, k)
        inverse_distances = 1 / (1e-6 + matches.distances)
        inverse_distances /= np.atleast_2d(inverse_distances.sum(axis=1)).T
        neighbors = self.outputs[matches.keys]
        return np.einsum("nij,ni->nj", neighbors, inverse_distances)
