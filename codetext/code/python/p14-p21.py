def p14(a):
    if not a:
        return []
    b = []
    for c in a:
        b.append(c)
        b.append(c)
    return b

def p15(a, b):
    if not a:
        return []
    c = []
    for d in a:
        for _ in range(b):
            c.append(d)
    return c

def p16(a, b):
    if b <= 0:
        return a
    c = []
    for i, d in enumerate(a):
        if (i + 1) % b != 0:
            c.append(d)
    return c

def p17(a, b):
    if b < 0:
        b = 0
    return [a[:b], a[b:]]

def p18(a, b, c):
    if b < 1:
        b = 1
    return a[b-1:c]

def p19(a, b):
    if not a:
        return []
    c = len(a)
    d = b % c
    return a[d:] + a[:d]

def p20(a, b):
    if b < 1 or b > len(a):
        return a
    return a[:b-1] + a[b:]

def p21(a, b, c):
    if c < 1:
        c = 1
    if c > len(b) + 1:
        c = len(b) + 1
    d = list(b)
    d.insert(c-1, a)
    return d
