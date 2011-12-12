def pascal(n):
    tri = [1]
    for _ in xrange(n + 1):
        yield tri
        tri = [1] + map(sum, zip(tri, tri[1:] + [0]))


def pretty_pascal(n):
    str_tris = ['  '.join(str(t) for t in tri) for tri in pascal(n)]
    width = len(str_tris[-1])
    for st in str_tris:
        diff = width - len(st)
        print ' ' * (diff // 2), st, ' ' * (diff // 2)
    return None


if __name__ == "__main__":
    from sys import argv
    pretty_pascal(int(argv[1]) if len(argv) > 1 else 10)
