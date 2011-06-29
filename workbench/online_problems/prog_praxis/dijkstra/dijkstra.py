#!/usr/bin/env python
"""
dijkstra.py

A conceptually simpler but less efficient implementation of Dijkstra's
Algorithm.
G:      digraph represented as a dictionary of dictionaries
D:      dictionary of distance estimates
P:      dictionary of predecessor estimates
s:      starting vertex
f:      finishing vertex
u, v:   vertices in G

GRE, 12/2010
"""

def init_single_source(G, D, P, s):
    """
    Sets up the digraph G for Dijkstra's Algorithm to find shortest paths from
    vertex s.
    """
    for v in G:
        D[v] = float("inf")
        P[v] = None
    D[s] = 0
    return

def relax(G, D, P, u, v):
    """
    If we've found a shorter path to v than that given by v's distance, update v
    accordingly.
    """
    if D[v] > D[u] + G[u][v]:
        D[v] = D[u] + G[u][v]
        P[v] = u
    return

def dijkstra(G, D, P, s):
    """
    Dijkstra's Algorithm for shortest paths from starting vertex s.
    """
    init_single_source(G, D, P, s)
    Q = list(G)
# Q will be sorted in ascending order by distance estimate
    Q.sort(key = lambda v: D[v], reverse = True)
    while Q:
        u = Q.pop()
        for v in G[u]:
            relax(G, D, P, u, v)
        Q.sort(key = lambda v: D[v], reverse = True)
    return

def find_shortest_path(G, D, P, s, f):
    """
    Build shortest path from predecessor subtree by traversing backwards from
    vertex f to s.
    """
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
