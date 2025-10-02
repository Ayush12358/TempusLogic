def p90_n_queens(a):
    b = []
    c = [[]]
    while c:
        d = c.pop()
        e = len(d)
        if e == a:
            b.append(d)
            continue
        for f in range(a):
            g = True
            for h, i in enumerate(d):
                if i == f or abs(h - e) == abs(i - f):
                    g = False
                    break
            if g:
                j = d + [f]
                c.append(j)
    return b

def p91_knights_tour(a):
    b = [[-1 for _ in range(a)] for _ in range(a)]
    c = [
        (2, 1), (1, 2), (-1, 2), (-2, 1),
        (-2, -1), (-1, -2), (1, -2), (2, -1)
    ]
    d, e = 0, 0
    b[d][e] = 0
    f = [(d, e)]
    def solve(g, h, i):
        if i == a * a:
            return True
        for j, k in c:
            l, m = g + j, h + k
            if 0 <= l < a and 0 <= m < a and b[l][m] == -1:
                b[l][m] = i
                f.append((l, m))
                if solve(l, m, i + 1):
                    return True
                b[l][m] = -1
                f.pop()
        return False
    if solve(d, e, 1):
        return f
    else:
        return None