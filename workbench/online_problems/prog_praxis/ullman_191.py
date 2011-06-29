#!/usr/bin/env python

def ullman(L, t, k):
    L.sort()
    if k * L[0] > t:
        return False
    elif sum(L[:k]) < t:
        return True
    else:
        return False

if __name__ == "__main__":
    L=[18.1,55.1,91.2,74.6,73.0,85.9,73.9,81.4,87.1,49.3,88.8,5.7,26.3,7.1,58.2,
        31.7,5.8,76.9,16.5,8.1,48.3,6.8,92.4,83.0,19.6]
    print ullman(L, 98.2, 3)
