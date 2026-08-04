"""Similarity search on numpy arrays via jax."""

from functools import partial

import jax
import jax.numpy as jnp


class L2Similarity:
    """Similarity search on numpy arrays using L2 distance via jax.

    Args:
        inputs: Input vectors through which to search.
        outputs: Output labels corresponding to the input vectors.

    Examples:

    >>> l2 = L2Similarity(
    ...    jnp.array([[0, 0, 0], [0.25, 0.25, 0.25], [1, 1, 1]]),
    ...    jnp.array([[0, 0], [0.25, 0.25], [1, 1]]),
    ... )
    >>> l2.search(jnp.array([[0, 0, 0], [1, 1, 0]]), k=1)
    Array([[0., 0.],
           [1., 1.]], dtype=float32)
    """

    def __init__(self, inputs: jax.Array, outputs: jax.Array):
        if inputs.shape[0] != outputs.shape[0]:
            raise ValueError("Input vectors and output vectors differ in 0th dimension")
        if inputs.dtype != jnp.float32 or outputs.dtype != jnp.float32:
            raise ValueError("Input and output must have dtype jnp.float32.")
        self.inputs = inputs
        self.half_inputs_norm_sq = jax.numpy.linalg.norm(inputs, axis=1) ** 2 / 2
        self.outputs = outputs

    def search(self, xs: jax.Array, k: int = 5, recall_target=0.95) -> jax.Array:
        return _search(
            xs,
            inputs=self.inputs,
            half_inputs_norm_sq=self.half_inputs_norm_sq,
            outputs=self.outputs,
            k=k,
            recall_target=recall_target,
        )


@partial(jax.jit, static_argnames=["k", "recall_target"])
def l2_ann(qy, db, half_db_norms, k=5, recall_target=0.95):
    """https://docs.jax.dev/en/latest/_autosummary/jax.lax.approx_min_k.html"""
    dists = half_db_norms - jax.lax.dot(qy, db.T)
    return jax.lax.approx_min_k(dists, k=k, recall_target=recall_target)


@jax.jit
def _nid(distances):
    inverse_distances = 1.0 / (1e-6 + distances)
    return (inverse_distances.T / inverse_distances.sum(axis=1)).T


@jax.jit
def _contract(ns, ids):
    return ns.T @ ids


@partial(jax.jit, static_argnames=["k", "recall_target"])
def _search(xs, *, inputs, half_inputs_norm_sq, outputs, k=5, recall_target=0.95):
    distances, indices = l2_ann(
        xs, inputs, half_inputs_norm_sq, k=k, recall_target=recall_target
    )
    inverse_distances = _nid(distances)
    neighbors = outputs[indices]
    return jax.vmap(_contract)(neighbors, inverse_distances)
