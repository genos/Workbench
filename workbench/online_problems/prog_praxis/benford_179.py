#!/usr/bin/env python

import csv

def benford(csv_file, numerical_position):
    """
    Calculates the percentages of first digit occurrence in numerical_position
    in the csv_file in question.
    """
    f = open(csv_file)
    d = dict(zip(range(1,10), [0] * 9))
    for row in csv.reader(f):
        try:
            d[int(row[numerical_position][0])] += 1
        except:
            pass
    total = float(sum(d.values()))
    for k in d:
        print "%s:\t% 3.3f%%" % (k, 100 * d[k] / total)
    f.close()
    return

if __name__ == '__main__':
    benford('lakes.csv', 3)
