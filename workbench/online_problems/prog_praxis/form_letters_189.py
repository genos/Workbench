#!/usr/bin/env python2.6

import csv
from string import digits

def form_letter(schema, data):
    reader = csv.reader(open(data))
    s = open(schema)
    lines = s.readlines()
    s.close()
    for row in reader:
        output = ''
        for line in lines:
            x = 0
            while x != -1:
                x = line.find("$", x)
                i = x+1
                while line[i] in digits:
                    i += 1
                if i != x+1:    # swap out $n with data's row[n]
                    n = int(line[x+1:i])
                    line = line[:x] + row[n] + line[i:]
                else:
                    break   # handles $$ case, as long as no other numbers are
                            # after it
            output = output + line
        print output + "\n"
    return

if __name__ == "__main__":
    form_letter("schema.txt", "data.txt")
