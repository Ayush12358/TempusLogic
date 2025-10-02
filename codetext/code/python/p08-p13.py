def p8(a):
    if not a:
        return []
    b = [a[0]]
    for i in range(1, len(a)):
        if a[i] != b[-1]:
            b.append(a[i])
    return b

def p9(a):
    if not a:
        return []
    b = []
    c = []
    for d in a:
        if not c or d == c[-1]:
            c.append(d)
        else:
            b.append(c)
            c = [d]
    if c:
        b.append(c)
    return b

def p10(a):
    if not a:
        return []
    b = p9(a)
    c = []
    for d in b:
        c.append([len(d), d[0]])
    return c

def p11(a):
    b = p10(a)
    c = []
    for d in b:
        if d[0] == 1:
            c.append(d[1])
        else:
            c.append(d)
    return c

def p12(a):
    b = []
    for c in a:
        if isinstance(c, list) and len(c) == 2 and isinstance(c[0], int):
            d, e = c
            for _ in range(d):
                b.append(e)
        else:
            b.append(c)
    return b

def p13(a):
    if not a:
        return []
    b = []
    c = 1
    for i in range(1, len(a)):
        if a[i] == a[i-1]:
            c += 1
        else:
            if c == 1:
                b.append(a[i-1])
            else:
                b.append([c, a[i-1]])
            c = 1
    if c == 1:
        b.append(a[-1])
    else:
        b.append([c, a[-1]])
    return b
