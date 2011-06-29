#!/usr/bin/env python

# euler59v1.py

import string

def xtoa(x, y):
    return chr(x ^ y)

def check_message(m):
# Chances are, the work 'the' should show up in you message somewhere
    return ' the ' in m

def check_candidacy(a, b, c, l):
    counter = 0
    for x in l:
        if not check_symbol(xtoa(a, x)):
            return False
        elif not check_symbol(xtoa(b, x)):
            return False
        elif not check_symbol(xtoa(c, x)):
            return False
        else:
            counter += 1
    return True

def message(a, b, c, l):
    message = ''
    counter = 0
    while counter < len(l):
        if counter % 3 == 0:
            message += xtoa(a, l[counter])
        elif counter % 3 == 1:
            message += xtoa(b, l[counter])
        else:
            message += xtoa(c, l[counter])
        counter += 1
    return message
    
def find_message():
    f = open('cipher1.txt')
    l = [int(n) for n in f.read().split(',')]
    f.close()
    for a in range(97, 123):
        for b in range(97, 123):
            for c in range(97, 123):
                m = message(a, b, c, l)
                if check_message(m):
                    return m
    return None

def sum_message_vals():
    m = find_message()
    return sum((ord(x) for x in m))

if __name__ == '__main__':
    print sum_message_vals()
