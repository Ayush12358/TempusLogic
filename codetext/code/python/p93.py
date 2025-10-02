from itertools import permutations, product

def p93_arithmetic_puzzle(a, b):
    c = ['+', '-', '*', '/']
    for d in set(permutations(a)):
        for e in product(c, repeat=len(d) - 1):
            f = str(d[0])
            g = float(d[0])
            try:
                for h in range(len(e)):
                    i = e[h]
                    j = d[h+1]
                    f = f"({f} {i} {j})"
                    if i == '+': g += j
                    elif i == '-': g -= j
                    elif i == '*': g *= j
                    elif i == '/':
                        if j == 0: raise ZeroDivisionError
                        g /= j
                if abs(g - b) < 1e-9:
                    return f"{f} = {b}"
            except ZeroDivisionError:
                pass
            if len(d) == 4:
                try:
                    k, l, m = e[0], e[1], e[2]
                    n, o, p, q = d[0], d[1], d[2], d[3]
                    r = 0
                    if k == '+': r = n + o
                    elif k == '-': r = n - o
                    elif k == '*': r = n * o
                    elif k == '/':
                        if o == 0: raise ZeroDivisionError
                        r = n / o
                    s = 0
                    if m == '+': s = p + q
                    elif m == '-': s = p - q
                    elif m == '*': s = p * q
                    elif m == '/':
                        if q == 0: raise ZeroDivisionError
                        s = p / q
                    t = 0
                    if l == '+': t = r + s
                    elif l == '-': t = r - s
                    elif l == '*': t = r * s
                    elif l == '/':
                        if s == 0: raise ZeroDivisionError
                        t = r / s
                    if abs(t - b) < 1e-9:
                        return f"(({n} {k} {o}) {l} ({p} {m} {q})) = {b}"
                except ZeroDivisionError:
                    pass
    return "No solution found"