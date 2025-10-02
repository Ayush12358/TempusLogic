def p1(a):
    if not a:
        return None
    return a[-1]

def p2(a):
    if len(a) < 2:
        return None
    return a[-2]

def p3(a, b):
    if b < 1 or b > len(a):
        return None
    return a[b-1]

def p4(a):
    return len(a)

def p5(a):
    return a[::-1]

def p6(a):
    return a == p5(a)

def p7(a):
    b = []
    c = list(a)
    while c:
        d = c.pop(0)
        if isinstance(d, list):
            c[0:0] = d
        else:
            b.append(d)
    return b
