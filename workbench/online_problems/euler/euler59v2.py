#!/usr/bin/env python

# euler59v2.py

def find_most_freq(some_list):
    d = {}
    for x in some_list:
        if x in d:
            d[x] += 1
        else:
            d[x] = 1
    max_key, max_val = 0, 0
    for k in d.keys():
        v = d[k]
        if v > max_val:
            max_key, max_val = k, v
    return max_key

def main():
    f = open('cipher1.txt')
    l = [int(n) for n in f.read().split(',')]
    f.close()
# We know that the cipher key consists of 3 letters; space should probably be
# the most common character in the message, so we select each part of the key to
# ensure this is the case.
# Note: ord(' ') = 32
    a = find_most_freq(l[::3]) ^ 32
    b = find_most_freq(l[1::3]) ^ 32
    c = find_most_freq(l[2::3]) ^ 32
    message = ''
    for i in range(len(l)):
        if i % 3 == 0:
            message += chr(l[i] ^ a)
        elif i % 3 == 1:
            message += chr(l[i] ^ b)
        else:
            message += chr(l[i] ^ c)
    return sum(ord(l) for l in message)

if __name__ == '__main__':
    print main()
