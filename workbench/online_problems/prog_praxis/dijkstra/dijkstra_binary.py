#!/usr/bin/env python

"""dijkstraMaster.py

A submission to Programming Praxis for Dijkstra's Algorithm. Uses a min
priority queue.
"""

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


class Vertex(object):

    def __init__(self, adj={}, dist=float("inf"), name=None, pred=None):
        self.adj = adj
        self.dist = dist
        self.name = name
        self.pred = pred
        return

    def __repr__(self):
        return self.name


class DiGraph(dict):

    def addEdge(self, u, v, weight):
        if u not in self:
            self[u] = Vertex(name=u, adj={})
        if v not in self:
            self[v] = Vertex(name=v, adj={})
        self[u].adj[v] = weight
        return

    def addEdges(self, edges):
        for e in edges:
            self.addEdge(e[0], e[1], e[2])
        return

    def _initSingleSource(self, s):
        for v in self:
            self[v].dist = float("inf")
            self[v].pred = None
        self[s].dist = 0
        return

    def _relax(self, u, v):
        d = self[u].dist + self[u].adj[v]
        if self[v].dist > d:
            self[v].dist = d
            self[v].pred = u
        return

    def dijkstra(self, s):
        self._initSingleSource(s)
        Q = BinaryMinHeap((self[v].dist, v) for v in self.iterkeys())
        while Q:
            u = Q.extractMin()
            for v in self[u].adj:
                self._relax(u, v)
                Q.decreaseKey(key=self[v].dist, node=v)
        return

    def findShortestPath(self, s, f):
        self.dijkstra(s)
        path, v = [f], self[f].pred
        while v is not None:
            path.append(v)
            v = self[v].pred
        path.reverse()
        return path


if __name__ == "__main__":
    G = DiGraph()
    G.addEdges([('a', 'c', 2), ('a', 'd', 6),
                ('b', 'a', 3), ('b', 'd', 8),
                ('c', 'd', 7), ('c', 'e', 5),
                ('d', 'e', 10)])
    print G.findShortestPath('a', 'e')
