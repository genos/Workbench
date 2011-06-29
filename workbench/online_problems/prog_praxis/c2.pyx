#################
# c2.pyx
# compile this with the help of setup.py (below), via:
# python setup.py build_ext --inplace
#################
cdef extern from "stdlib.h":
    int rand()


cdef inline int die():
    return 1 + rand() % 6


cpdef single_game(chutes, ladders):
    cdef int pos
    pos = 0
    path = []
    while pos < 100:
        pos += die()
        if pos in chutes:
            pos = chutes[pos]
        elif pos in ladders:
            pos = ladders[pos]
        path.append(pos)
    if path[-1] > 100:
        path[-1] = 100
    return path


cpdef mult_games(int n, chutes, ladders):
    cdef int i
    mgs = []
    for 0 <= i < n:
        mgs.append(len(single_game(chutes, ladders)))
    return mgs


cpdef compete(int k, int n, chutes, ladders):
    return [min(mult_games(k, chutes, ladders)) for _ in xrange(n)]


cdef inline float mean(xs, int n):
    cdef float s
    cdef int i
    for 0 <= i < n:
        s += xs[i]
    return s / n


cpdef stats(k, n, chutes, ladders):
    games = compete(k, n, chutes, ladders)
    return min(games), max(games), mean(games, n)

chutes = {16: 6, 47: 26, 49: 11, 56: 53, 62: 19, 64: 60, 87: 24, 93: 73, 95: 75,
            98: 78}
ladders = {1: 38, 4: 14, 9: 31, 21: 42, 28: 84, 36: 44, 51: 67, 71: 91, 80: 100}

#################
# setup.py
#################
from distutils.core import setup
from distutils.extension import Extension
from Cython.Distutils import build_ext

ext_modules=[
        Extension("c2",
                  ["c2.pyx"])
]

setup(
    cmdclass = {'build_ext': build_ext},
    ext_modules = ext_modules
)
