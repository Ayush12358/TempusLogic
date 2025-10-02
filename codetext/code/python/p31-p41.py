import math
import time

def p31(a):
    if a < 2:
        return False
    if a == 2:
        return True
    if a % 2 == 0:
        return False
    b = 3
    while b * b <= a:
        if a % b == 0:
            return False
        b += 2
    return True

def p32(a, b):
    while b:
        a, b = b, a % b
    return a

def p33(a, b):
    return p32(a, b) == 1

def p34(a):
    if a == 1:
        return 1
    b = 0
    for c in range(1, a):
        if p33(c, a):
            b += 1
    return b

def p35(a):
    b = []
    while a % 2 == 0:
        b.append(2)
        a //= 2
    c = 3
    while c * c <= a:
        while a % c == 0:
            b.append(c)
            a //= c
        c += 2
    if a > 2:
        b.append(a)
    return b

def p36(a):
    if a == 1:
        return []
    b = p35(a)
    if not b:
        return []
    c = []
    d = 1
    for i in range(1, len(b)):
        if b[i] == b[i-1]:
            d += 1
        else:
            c.append([b[i-1], d])
            d = 1
    c.append([b[-1], d])
    return c

def p37(a):
    if a == 1:
        return 1
    b = p36(a)
    c = 1
    for d, e in b:
        c *= (d - 1) * (d ** (e - 1))
    return c

def p38(a):
    print(f"\nEvaluating phi({a}) using the solution for P34:")
    b = time.time()
    c = p34(a)
    d = time.time()
    print(f"Result: {c}, Time: {d - b:.6f}s")

    print(f"\nEvaluating phi({a}) using the solution for P37:")
    b = time.time()
    c = p37(a)
    d = time.time()
    print(f"Result: {c}, Time: {d - b:.6f}s")

def p39(a, b):
    c = []
    for d in range(a, b + 1):
        if p31(d):
            c.append(d)
    return c

def p40(a):
    if a <= 2 or a % 2 != 0:
        return None
    b = p39(2, a)
    for c in b:
        d = a - c
        if p31(d):
            return [c, d]
    return None

def p41(a, b, c=0):
    if a % 2 != 0:
        a += 1
    for d in range(a, b + 1, 2):
        e = p40(d)
        if e and e[0] > c:
            print(f"{d} = {e[0]} + {e[1]}")

