#!/usr/bin/env python
"""
dijkstra2.py

A slightly faster implementation of Dijkstra's Algorithm, using a linear search
through Q to find the vertex of minimum distance.

GRE, 12/2010
"""

def init_single_source(G, D, P, s):
    for v in G:
        D[v] = float("inf")
        P[v] = None
    D[s] = 0
    return

def relax(G, D, P, u, v):
    if D[v] > D[u] + G[u][v]:
        D[v] = D[u] + G[u][v]
        P[v] = u
    return

def extract_min(D, Q):
    min_key, min_val = Q[0], D[Q[0]]
    for key in Q:
        if D[key] < min_val:
            min_key, min_val = key, D[key]
    Q.remove(min_key)
    return min_key

def dijkstra(G, D, P, s):
    init_single_source(G, D, P, s)
    Q = list(G)
    while Q:
        u = extract_min(D, Q)
        for v in G[u]:
            relax(G, D, P, u, v)
    return

def find_shortest_path(G, D, P, s, f):
    dijkstra(G, D, P, s)
    path = [f]
    v = P[f]
    while v is not None:
        path.append(v)
        v = P[v]
    path.reverse()
    return path

if __name__ == "__main__":
    G = {'a': {'c':2, 'd':6},
         'b': {'d':8, 'a':3},
         'c': {'d':7, 'e':5},
         'd': {'e': 10},
         'e': {}}
    D = dict.fromkeys(G)
    P = dict.fromkeys(G)
    print find_shortest_path(G, D, P, 'a', 'e')
