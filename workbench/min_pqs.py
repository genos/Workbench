#!/usr/bin/env python

"""min_pqs.py

Trying to grow a collection of minimum priority queues.

GRE, 12/2010
"""


class SortEachTimeHeap(list):

    def __init__(self, nodes=[]):
        """index{} will keep track of indices of nodes"""
        list.__init__(self, nodes)
        self.index = {}
        for i in xrange(len(self)):
            self.index[self[i]] = i
        self.sort(reverse=True)
        return

    def decreaseKey(self, node=None, key=None):
        if node in self.index:
            i = self.index[node]
            self[i][0] = key
            self.sort(reverse=True)
        return

    def extractMin(self):
        return self.pop()[1]


left = lambda i: 2 * i + 1
parent = lambda i: i // 2
right = lambda i: 2 * i + 2


class BinaryMinHeap(list):

    def __init__(self, nodes=[]):
        list.__init__(self, nodes)
        self.index = {}
        for i in xrange(len(self)):
            self.index[self[i]] = i
        self._buildMinHeap()
        return

    def extractMin(self):
        self[0], self[-1] = self[-1], self[0]
        m = self.pop()
        self._minHeapify(0)
        return m[1]

    def decreaseKey(self, key=None, node=None):
        if node in self.index:
            i = self.index[node]
            self[i][0] = key
            while i > 0 and self[parent(i)] > self[i]:
                self[i], self[parent(i)] = self[parent(i)], self[i]
                self.index[node] = i
                i = parent(i)
        return

    def _buildMinHeap(self):
        p = parent(len(self) - 1)       # last parent node in heap
        for i in xrange(p, -1, -1):
            self._minHeapify(i)
        return

    def _minHeapify(self, i=-1):
        n = len(self) - 1
        if i == -1:
            i = n
        l, r, m = left(i), right(i), i
        if l < n and self[l] < self[i]:
            m = l
        if r < n and self[r] < self[m]:
            m = r
        if m != i:
            self[i], self[m] = self[m], self[i]
            self._minHeapify(m)
        return
