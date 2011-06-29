#!/usr/bin/env python
"""
house_of_reps.py

My solution to programming praxis exercise available at
http://programmingpraxis.com/2011/04/12/house-of-representatives/
Uses the Huntington-Hill method to build # representatives for each state.

GRE, 4/12/11
"""
from csv import reader
from math import sqrt


def g(n, p):
    """
    Geometric mean used in Huntington-Hill method.
    """
    return p / sqrt(n * (n + 1))


def make_state(name, pop):
    """
    We'll represent states as dictionaries holding appropriate data.
    """
    return {"name": name, "pop": pop, "geo_mean": g(1, pop), "reps": 1}


def reps(data):
    """
    Builds states from data, iteratively adds house reps.
    """
    states = [make_state(*d) for d in data]
    for _ in xrange(435 - 50):
        m = max(states, key=lambda s: s["geo_mean"])
        m["reps"] += 1
        m["geo_mean"] = g(m["reps"], m["pop"])
    return states


def maine(pop_file="pop_change.csv"):
    """
    Runs house_reps() on data given in pop_file (depends on format!!), outputs
    results.
    """
    with open(pop_file) as f:
        pop_data = ((r[0], int(r[11])) for r in reader(f) if str.isdigit(r[11])
                and r[0] != "District of Columbia")
        for s in sorted(reps(pop_data), key=lambda s: (-s["reps"], s["name"])):
            print "{0}:{1:>{space}d}".format(s["name"], s["reps"],
                    space=20 - len(s["name"]))
    return None


if __name__ == "__main__":
    maine()
