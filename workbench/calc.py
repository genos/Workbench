#!/usr/bin/env python

def calc():
    try:
        while True:
            a = oct(eval(raw_input(">>> ")))
            print "Answer is: %s" % a[1:]
    except:
        return

if __name__ == "__main__":
    calc()
