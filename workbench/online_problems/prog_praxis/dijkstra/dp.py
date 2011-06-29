#!/usr/bin/env python


def make_heap():
    return []


def find_min(h):
    return h[0]


def insert(x, h):
    return merge([x], h)


def merge(h1, h2):
    if h1 == []:
        return h2
    elif h2 == []:
        return h1
    elif h1[0] < h2[0]:
        return [h1[0], h2 + h1[1:]]
    else:
        return [h2[0], h1 + h2[1:]]


def merge_pairs(h):
    if h == []:
        return h
    elif len(h) == 1:
        return h[0]
    else:
        return merge(merge(h[0], h[1]), merge_pairs(h[2:]))


def delete_min(h):
    if h == []:
        raise IndexError
    elif len(h[1:]) == 0:
        return []
    elif len(h[1:]) == 1:
        return h[1:]
    else:
        return merge_pairs(h[1:])


class Vertex(object):

    def __init__(self, adj={}, dist=float("inf"), name=None, pred=None):
        self.adj = adj
        self.dist = dist
        self.name = name
        self.pred = pred
        return

    def __lt__(self, other):
        return self.dist < other.dist


def add_edge(D, u, v, weight):
    if u not in D:
        D[u] = Vertex(name=u, adj={})
    if v not in D:
        D[v] = Vertex(name=v, adj={})
    D[u].adj[v] = weight
    return


def add_edges(D, edges):
    for edge in edges:
        add_edge(D, edge[0], edge[1], edge[2])
    return


def init_single_source(D, s):
    for v in D:
        D[v].dist = float("inf")
        D[v].pred = None
    D[s].dist = 0
    return


def relax(D, u, v):
    d = D[u].dist + D[u].adj[v]
    if D[v].dist > d:
        D[v].dist = d
        D[v].pred = u
    return


def dijkstra(D, s):
    init_single_source(D, s)
    Q = make_heap()
    for v in D.values():
        Q = insert(v, Q)
    while Q:
        u, Q = find_min(Q), delete_min(Q)
        for v in u.adj:
            relax(D, u.name, v)
        Q = merge_pairs(Q)
    return


def find_shortest_path(D, s, f):
    dijkstra(D, s)
    path, v = [f], D[f].pred
    while v is not None:
        path.append(v)
        v = D[v].pred
    path.reverse()
    return path


if __name__ == "__main__":
    D = {}
    add_edges(D, [('a', 'c', 2), ('a', 'd', 6),
                  ('b', 'a', 3), ('b', 'd', 8),
                  ('c', 'd', 7), ('c', 'e', 5),
                  ('d', 'e', 10)])
    print find_shortest_path(D, 'a', 'e')
