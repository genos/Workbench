#!/usr/bin/env python2.6

def is_str_subset(str1, str2):
    d = {}      # dict of letter --> count for letter in str2
    for letter in str2:
# theta(m) where m = |str2|; most expensive part here, but cheaper on its own
# than sorting m first to quickly search [theta(m lg m)] and same as linearly
# searching unsorted str2
        try:
            d[letter] += 1
        except KeyError:
            d[letter] = 1
    for letter in str1:
# O(n) where n = |str1|; n < m, so total work is theta(m)
        try:
            if d[letter] == 0:
                return False
            else:
                d[letter] -= 1
        except KeyError:
            return False
    return True


if __name__ == '__main__':
    str1 = "DA"
    str2 = "ABCD"
    str3 = "DAD"
    print "Is str1 a string subset of str2?\t%s" % is_str_subset(str1, str2)
    print "Is str3 a string subset of str2?\t%s" % is_str_subset(str3, str2)
