n = open('network.txt')
V = set()
E = {}

for line in n.readlines():
    n1, n2, w = line.strip().split(' ')
    V.add(n1); V.add(n2)
    try:
        E[n1][n2] = RR(w)
    except KeyError:
        E[n1] = {n2: RR(w)}

n.close()

G = DiGraph(E)
d, p = G.shortest_path_all_pairs()
print d
print p
