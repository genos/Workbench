#!/usr/bin/env python
# Find the longest palindrome in a given text

def is_pal(s):
    return s == s[::-1]

def flp(s):
    longest, len_longest  = 'g', 1
    for i in xrange(len(s)):
        for j in xrange(1,len(s[i:])):
            sub_s = s[i:i+j+1]
            if is_pal(sub_s) and len(sub_s) > len_longest:
                longest, len_longest = sub_s, j+1
    return (longest, len_longest)

def main():
    f = open('four_score.txt')
    lines =  ''.join(l for l in f.read().split())
    f.close()
    pal, length = flp(lines)
    print "Length of longest palindrome:\t%s" % length
    print "Longest palindrome:\t%s" % pal
    return

if __name__ == '__main__':
    main()
