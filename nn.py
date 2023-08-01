#!/usr/bin/env python
"""nn.py
A simple feed-forward neural network, inspired by:
- iamtrask.github.io/2015/07/12/basic-python-network/
- rolisz.ro/2013/04/18/neural-networks-in-python
- mattmazur.com/2015/03/17/a-step-by-step-backpropagation-example/
"""
from dataclasses import dataclass, field
import numpy as np
from numpy.typing import NDArray


def sigma(x):
    """Logistic activation function."""
    return 1 / (1 + np.exp(-x))


def d_sigma(y):
    """Derivative of logistic function; assumes that y is σ(x) for some x, so
    this is σ(x) * (1 - σ(x))
    """
    return y * (1 - y)


@dataclass
class NeuralNetwork:
    """A simple neural network implementation.

    Attributes:
        shape (list of integers, required): the shape of our network
        learning rate (positive float): constant multiplier of our gradient step
        iterations (positive integer): number of rounds in training
        seed (integer): seed for random state
        weights (list of arrays, set in `__post_init__`): weights of our network
    """

    shape: list[int]
    learning_rate: float = 1.0
    iterations: int = int(1e4)
    seed: int = 1729
    weights: list[NDArray[np.float64]] = field(init=False)

    def __post_init__(self):
        """Validate attributes, then initialize `weights`"""
        # error checking
        assert np.ndim(self.shape) == 1, f"{self.shape} is not one dimensional"
        assert len(self.shape) > 2, f"{self.shape} too short; length should be > 2"
        assert all(
            isinstance(s, int) and s > 0 for s in self.shape
        ), f"{self.shape} should contain only positive integers"
        assert (
            isinstance(self.learning_rate, float) and self.learning_rate > 0
        ), f"{self.learning_rate} is not a positive real number"
        assert (
            isinstance(self.iterations, int) and self.iterations > 0
        ), f"{self.iterations} is not a positive integer"
        assert isinstance(self.seed, int), f"{self.seed} is not an integer"
        # initialize weights
        rng = np.random.default_rng(self.seed)
        self.weights = [
            rng.uniform(low=-1, high=1, size=(row, col))
            for row, col in zip(self.shape, self.shape[1:])
        ]

    def __repr__(self):
        """Pretty printing"""
        w_str = "\n".join(str(w) for w in self.weights)
        return f"""NeuralNetwork
shape:              {self.shape}
learning rate:      {self.learning_rate}
iterations:         {self.iterations}
weights:
{w_str}"""

    def fit(self, X, y):
        """Use X and y to train our neural network.

        Args:
            X (array-like, two dimensional): training input values
            y (array-like, two dimensional): training output values

        Notes:
            Shape requirements:
                X.shape[1] == self.shape[0]
                y.shape[1] == self.shape[-1]
                X.shape[0] == y.shape[0]
        """
        # conversion, if necessary
        X = np.asarray(X)
        y = np.asarray(y)
        # error checking
        assert np.ndim(X) == 2, "input should be two dimensional"
        assert np.ndim(y) == 2, "output should be two dimensional"
        assert X.shape[1] == self.shape[0], "input shape doesn't match"
        assert y.shape[1] == self.shape[-1], "output shape doesn't match"
        assert X.shape[0] == y.shape[0], "input and output shapes don't match"
        # result of feeding data through each layer
        output = [np.zeros((X.shape[0], s)) for s in self.shape]
        output[0] = X
        # deltas for updating weights
        delta = [np.zeros_like(w) for w in self.weights]
        for _ in range(self.iterations):
            # feed forward
            for i, w in enumerate(self.weights):
                output[i + 1] = sigma(output[i] @ w)
            # backpropagate
            delta[-1] = (y - output[-1]) * d_sigma(output[-1])
            for i in range(len(self.shape) - 3, -1, -1):
                delta[i] = delta[i + 1] @ self.weights[i + 1].T * d_sigma(output[i + 1])
            for i, (o, d) in enumerate(zip(output, delta)):
                self.weights[i] += self.learning_rate * o.T @ d

    def predict(self, X):
        """Predict output given new input X.

        Args:
            X (numpy array): new input values

        Returns:
            numpy array: predicted y_hat for given X

        Notes:
            Shape requirements:
                ndim(X) <= 2
                if len(X.shape) == 1:
                    X.shape[0] == self.shape[0]
                else:
                    X.shape[1] == self.shape[0]
        """
        # conversion, if necessary
        X = np.asarray(X)
        # error checking
        assert np.ndim(X) <= 2, "input should be at most two dimensional"
        assert (
            X.shape[0 if np.ndim(X) == 1 else 1] == self.shape[0]
        ), "input shape doesn't match"
        # feed forward
        y_hat = X
        for w in self.weights:
            y_hat = sigma(y_hat @ w)
        return y_hat


if __name__ == "__main__":
    NN = NeuralNetwork([2, 7, 4, 5, 1])
    X = [[0, 0], [0, 1], [1, 0], [1, 1]]
    y = [[0], [1], [1], [0]]
    NN.fit(X, y)
    for x in X:
        print(f"{x}: {NN.predict(x)}")
