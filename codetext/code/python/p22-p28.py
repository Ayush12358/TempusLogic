import random
import itertools
from collections import Counter

def p22(a, b):
    if a <= b:
        return list(range(a, b + 1))
    else:
        return list(range(a, b - 1, -1))

def p23(a, b):
    if b < 0:
        return []
    return random.sample(a, b)

def p24(a, b):
    if a < 0 or b < 1 or a > b:
        return []
    c = range(1, b + 1)
    return random.sample(c, a)

def p25(a):
    return random.sample(a, len(a))

def p26(a, b):
    if a < 0:
        return []
    return list(itertools.combinations(b, a))

def p27_group(a, b):
    if not b:
        yield []
        return
    c = b[0]
    d = b[1:]
    for e in itertools.combinations(a, c):
        f = [g for g in a if g not in e]
        for h in p27_group(f, d):
            yield [list(e)] + h

def p27a(a):
    if len(a) != 9:
        return []
    b = [2, 3, 4]
    c = list(set(itertools.permutations(b)))
    d = []
    for e in c:
        for f in p27_group(a, e):
            d.append(f)
    return d

def p27b(a, b):
    if sum(b) != len(a):
        return []
    c = list(set(itertools.permutations(b)))
    d = []
    for e in c:
        for f in p27_group(a, e):
            d.append(f)
    return d

def p28a(a):
    return sorted(a, key=len)

def p28b(a):
    if not a:
        return []
    b = [len(c) for c in a]
    c = Counter(b)
    return sorted(a, key=lambda d: c[len(d)])
