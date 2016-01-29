#!/usr/bin/env python
# coding: utf-8
"""nn.py
A neural network in Python, inspired by
iamtrask.github.io/2015/07/12/basic-python-network/
and
rolisz.ro/2013/04/18/neural-networks-in-python
and
mattmazur.com/2015/03/17/a-step-by-step-backpropagation-example/
"""
import numpy as np


def sigma(x):
    """Logistic activation function."""
    return 1 / (1 + np.exp(-x))


def d_sigma(y):
    """Derivative of logistic function; assumes that y is sigma(x) for some x,
    so this is sigma(x) * (1 - sigma(x))
    """
    return y * (1 - y)


class NeuralNetwork(object):
    """A simple neural network implementation."""

    def __init__(self, shape, learning_rate=1.0, iterations=int(1e4)):
        """A simple neural network implementation.

        Parameters
        ----------
        shape : array-like, one dimensional, consisting of positive integers,
        length > 2
            the shape of our network; the number of nodes in each layer.

        learning rate : positive float
            constant multiplier of our gradient step

        iterations : positive integer
            number of rounds in training
        """
        # error checking
        assert isinstance(shape, (tuple, list, np.ndarray)), \
            "{0} is not array-like".format(shape)
        assert len(np.shape(shape)) == 1, \
            "{0} is not one dimensional"
        assert len(shape) > 2, \
            "{0} too short; length should be > 2".format(shape)
        assert all(isinstance(s, int) and s > 0 for s in shape), \
            "{0} should contain only positive integers".format(shape)
        assert isinstance(learning_rate, float) and learning_rate > 0, \
            "{0} is not a positive real number".format(learning_rate)
        assert isinstance(iterations, int) and iterations > 0, \
            "{0} is not a positive integer".format(iterations)
        # set attributes
        self.shape = shape
        self.length = len(shape) - 1
        self.learning_rate = learning_rate
        self.iterations = iterations
        self.weights = [np.random.uniform(low=-1,
                                          high=1,
                                          size=(row, col))
                        for row, col in zip(self.shape, self.shape[1:])]

    def __len__(self):
        """Length = length of weights = one less than length of shape"""
        return self.length

    def __repr__(self):
        """Pretty printing"""
        return """NeuralNetwork
shape:              {0}
learning rate:      {1}
iterations:         {2}
weights:
{3}""".format(self.shape, self.learning_rate, self.iterations,
              '\n'.join(str(w) for w in self.weights))

    def fit(self, X, y):
        """Use X and y to train our neural network.

        Parameters
        ----------
        X : array-like, two dimensional
            training input values

        y : array-like, two dimensional
            training output values

        Notes
        -----
        Shape requirements:
            X.shape[1] == self.shape[0]
            y.shape[1] == self.shape[-1]
            X.shape[0] == y.shape[0]
        """
        # conversion, if necessary
        if not isinstance(X, np.ndarray):
            X = np.array(X)
        if not isinstance(y, np.ndarray):
            y = np.array(y)
        # error checking
        assert len(X.shape) == 2, "input should be two dimensional"
        assert len(y.shape) == 2, "output should be two dimensional"
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
                output[i + 1] = sigma(output[i].dot(w))
            # backpropagate
            delta[-1] = (y - output[-1]) * d_sigma(output[-1])
            for i in range(self.length - 2, -1, -1):
                delta[i] = delta[i + 1].dot(self.weights[i + 1].T) * \
                           d_sigma(output[i + 1])
            for i, (o, d) in enumerate(zip(output, delta)):
                self.weights[i] += self.learning_rate * o.T.dot(d)

    def predict(self, X):
        """Predict output given new input X.

        Parameters
        ----------
        X : numpy array
            new input values

        Returns
        -------
        Predicted y_hat for given X.

        Notes
        -----
        Shape requirements:
            len(X.shape) <= 2
            if len(X.shape == 1:
                X.shape[0] == self.shape[0]
            else:
                X.shape[1] == self.shape[0]
        """
        # conversion, if necessary
        if not isinstance(X, np.ndarray):
            X = np.array(x)
        # error checking
        assert len(X.shape) <= 2, "input should be at most two dimensional"
        assert X.shape[0 if len(X.shape) == 1 else 1] == self.shape[0], \
            "input shape doesn't match"
        # feed forward
        y_hat = X
        for w in self.weights:
            y_hat = sigma(y_hat.dot(w))
        return y_hat


if __name__ == "__main__":
    NN = NeuralNetwork([2, 7, 4, 5, 1])
    X = [[0, 0], [0, 1], [1, 0], [1, 1]]
    y = [[0], [1], [1], [0]]
    NN.fit(X, y)
    for x in X:
        print("{0}: {1}".format(x, NN.predict(x)))
