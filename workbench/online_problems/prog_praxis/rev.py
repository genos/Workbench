#!/usr/bin/env python


def reverse_slice(s, i, j):
    r = s[:i]
    for k in xrange(i, j):
        r += s[j - k + i - 1]
    r += s[j:]
    return r


def rev_slice_list(s, i, j):
    s = list(s)
    for k in xrange((j - i) / 2):
        s[i + k], s[j - k - 1] = s[j - k - 1], s[i + k]
    r = ""
    for l in s:
        r += l
    return r


def find_space(s, i):
    for j in xrange(i, len(s)):
        if s[j] == ' ':
            return j
    return len(s)


def reverse_words(s):
    r = reverse_slice(s, 0, len(s))     # or rev_slice_list(s, 0, len(s))
    i, j = 0, find_space(r, 0)
    while j is not len(r):
        r = reverse_slice(r, i, j)      # or rev_slice_list(s, i, j)
        i, j = j + 1, find_space(r, j + 1)
    return reverse_slice(r, i, j)


if __name__ == "__main__":
    for s in ["", " ", "  ", "hello", "hello ", " hello",
                "the quick brown fox", "the quick  brown fox",
                "the quick  brown 42 fox!"]:
        print '> (reverse-words "%s")\n"%s"' % (s, reverse_words(s))

"""
Python strings are immutable, so we can't change them in place. I came up with
two options, neither of which are terribly satisfactory:
1. build a new string with the slice reversed (<code>reverse_slice()</code>) or
2. turn our string into a list and work on that (<code>rev_slice_list()</code>
   like Remco's solution with arrays), but that still creates a new object
   and requires converting back to a string.
I decided to emulate the official solution's sample tests somewhat closely.
[sourcecode lang="python"]
#!/usr/bin/env python


def reverse_slice(s, i, j):
    r = s[:i]
    for k in xrange(i, j):
        r += s[j - k + i - 1]
    r += s[j:]
    return r


def rev_slice_list(s, i, j):
    s = list(s)
    for k in xrange((j - i) / 2):
        s[i + k], s[j - k - 1] = s[j - k - 1], s[i + k]
    r = ""
    for l in s:
        r += l
    return r


def find_space(s, i):
    for j in xrange(i, len(s)):
        if s[j] == ' ':
            return j
    return len(s)


def reverse_words(s):
    r = reverse_slice(s, 0, len(s))     # or rev_slice_list(s, 0, len(s))
    i, j = 0, find_space(r, 0)
    while j is not len(r):
        r = reverse_slice(r, i, j)      # or rev_slice_list(s, i, j)
        i, j = j + 1, find_space(r, j + 1)
    return reverse_slice(r, i, j)


if __name__ == "__main__":
    for s in ["", " ", "  ", "hello", "hello ", " hello",
                "the quick brown fox", "the quick  brown fox",
                "the quick  brown 42 fox!"]:
        print '> (reverse-words "%s")\n"%s"' % (s, reverse_words(s))
[/sourcecode]
"""
