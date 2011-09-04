#!/usr/bin/env python


def segments(s, d):
    segs = []
    while s:
        if s in d:
            segs.append(s)
            s = ""
        for i in xrange(1, len(s)):
            if s[:i] in d:
                segs.append(s[:i])
                s = s[i:]
                break
            if i == len(s) - 1:    # already tested full string s
                s = ""
    return " ".join(segs)


if __name__ == "__main__":
    d = set(["a", "aa", "aaa", "ab", "apple", "apricot", "is", "pie", "test",
             "this"])
    print segments("thisisatest", d)
    print segments("aaab", d)
