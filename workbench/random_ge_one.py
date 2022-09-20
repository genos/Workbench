#!/usr/bin/env python3

"""random_ge_one.py

Add up uniformly distributed random numbers between zero and one, counting how
many you need to have your sum >= 1. An interesting thing happens if you do
this n times and take the average (for large values of n)...

See explanation here, for spoilers:
http://www.mostlymaths.net/2010/08/and-e-appears-from-nowhere.html

GE, 2/9/11
"""

# Used for reading command line arguments
import sys

# Gives (approximately) uniformly distributed random real numbers in [0, 1]
from random import random


# Adds random numbers until sum >= 1; returns the number of iterations needed
def single_run():
    total = iters = 0
    while total < 1:
        total += random()
        iters += 1
    return iters


# Takes the average of n "single runs"
def n_runs_average(n):
    total = 0.0
    for i in xrange(n):
        total += single_run()
    return total / n


# Main execution
if __name__ == "__main__":
    try:
        print(n_runs_average(int(sys.argv[1])))
    except:
        print(n_runs_average(1000))
