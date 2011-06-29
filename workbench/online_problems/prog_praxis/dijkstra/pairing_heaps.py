def make_heap():
    return []

def find_min(h):
    return h[0]

def insert(x, h):
    return merge([x], h)

def merge(h1, h2):
    if h1 == []:
        return h2
    elif h2 == []:
        return h1
    elif h1[0] < h2[0]:
        return [h1[0], h2 + h1[1:]]
    else:
        return [h2[0], h1 + h2[1:]]

def merge_pairs(h):
    if h == []:
        return h
    elif len(h) == 1:
        return h[0]
    else:
        return merge(merge(h[0], h[1]), merge_pairs(h[2:]))

def delete_min(h):
    if h == []:
        raise IndexError
    elif len(h[1:]) == 0:
        return []
    elif len(h[1:]) == 1:
        return h[1:]
    else:
        return merge_pairs(h[1:])
