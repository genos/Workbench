#!/usr/bin/env python2.6
"""
top_sort.py

We implement a topological sort of a directed acyclic graph (DAG) G, implemented
as a dictionary of edge lists. We first check if G is acyclic, then sort it if
so. I got some help from Wikipedia
(http://en.wikipedia.org/wiki/Topological_sorting), CLRS3e, and the original
blog post.

http://programmingpraxis.com/2010/11/19/topological-sort/

GRE, 11/19/10
"""

from copy import deepcopy

def predless_nodes(G):
# Nodes without a predecessor
    S = G.keys()
    for n in G:
        for m in G[n]:
            if m in S: S.remove(m)
    return S

def leaves(G):
# Nodes with no sucessor
    return [n for n in G if G[n] == []]

def is_cyclic(G):
# Uses the algorithm described in the blog post for determining if G is cyclic
    L = leaves(G)
    if len(G) == 0:
        return False
    elif len(L) == 0:
        return True
    else:
        for l in L:
            for n in G.iterkeys():
                if l in G[n]:
                    G[n].remove(l)
            G.pop(l)
        return is_cyclic(G)
    
def topological_sort(G):
# Uses depth-first search from CLRS3e and Wikipedia
    if is_cyclic(deepcopy(G)):      # deepcopy or else we lose G
        return "Error! G was cyclic."
    v = dict.fromkeys(G)
    T = []
    S = predless_nodes(G)
# Define depth-first search
    def visit(n):
        if not v[n]:
            v[n] = True
            for m in G[n]:
                visit(m)
            T.insert(0,n)
        return
# Execute depth-first search
    for n in S:
        visit(n)
    return T

    
if __name__ == '__main__':
    def tester(G):
        print "Graph:\n%s" % G
        print "A topological sort of this graph:\n%s" % topological_sort(G)
        return

    G1 = {2:[], 3:[8, 10], 5:[11], 7:[8,11], 8:[9], 9:[], 10:[], 11:[2, 9, 10]}
    tester(G1)
    G2 = {2:[], 3:[8, 10], 5:[11], 7:[8,11], 8:[9], 9:[], 10:[5], 11:[2, 9, 10]}
    tester(G2)
