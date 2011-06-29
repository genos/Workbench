#!/usr/bin/env python


# Problem 1
def store_credit(C, L):
    for i in xrange(len(L) - 1):
        try:
            i2 = L.index(C - L[i], i + 1)
            return [i + 1, i2 + 1]
        except ValueError:
            pass
    return None


# Problem 2
def reverse_words(in_str):
    return ' '.join(w for w in in_str.split(' ')[::-1])


# Problem 3
t9_dict = {'a': '2', 'b': '22', 'c': '222', 'd': '3', 'e': '33', 'f': '333',
            'g': '4', 'h': '44', 'i': '444', 'j': '5', 'k': '55', 'l': '555',
            'm': '6', 'n': '66', 'o': '666', 'p': '7', 'q': '77', 'r': '777',
            's': '7777', 't': '8', 'u': '88', 'v': '888', 'w': '9', 'x': '99',
            'y': '999', 'z': '9999'}


def t9_spelling(message):
    t9_message, prev = '', ' '
    for char in message:
        if char == ' ':
            t9_message += '0'
            prev = '0'
        elif prev in t9_dict[char]:
            t9_message += (' ' + t9_dict[char])
            prev = t9_dict[char][0]
        else:
            t9_message += (t9_dict[char])
            prev = t9_dict[char][0]
    return t9_message


"""
I'm sure there's a cleverer way to solve the store credit question, but it
eludes me this morning. Although my solutions look very similar to the official
ones, I didn't look ahead this time, scout's honor.
[sourcecode lang="python"]
#!/usr/bin/env python


# Problem 1
def store_credit(C, L):
    for i in xrange(len(L) - 1):
        try:
            i2 = L.index(C - L[i], i + 1)
            return [i + 1, i2 + 1]
        except ValueError:
            pass
    return None


# Problem 2
def reverse_words(in_str):
    return ' '.join(w for w in in_str.split(' ')[::-1])


# Problem 3
t9_dict = {'a': '2', 'b': '22', 'c': '222', 'd': '3', 'e': '33', 'f': '333',
            'g': '4', 'h': '44', 'i': '444', 'j': '5', 'k': '55', 'l': '555',
            'm': '6', 'n': '66', 'o': '666', 'p': '7', 'q': '77', 'r': '777',
            's': '7777', 't': '8', 'u': '88', 'v': '888', 'w': '9', 'x': '99',
            'y': '999', 'z': '9999'}


def t9_spelling(message):
    t9_message, prev = '', ' '
    for char in message:
        if char == ' ':
            t9_message += '0'
            prev = '0'
        elif prev in t9_dict[char]:
            t9_message += (' ' + t9_dict[char])
            prev = t9_dict[char][0]
        else:
            t9_message += (t9_dict[char])
            prev = t9_dict[char][0]
    return t9_message
[/sourcecode]
"""
