def p97_sudoku_solver(a):
    def find_empty_cell():
        for b in range(9):
            for c in range(9):
                if a[b][c] == 0:
                    return (b, c)
        return None
    def is_valid(d, e):
        f, g = e
        if d in a[f]:
            return False
        if d in [a[h][g] for h in range(9)]:
            return False
        i, j = 3 * (f // 3), 3 * (g // 3)
        for k in range(i, i + 3):
            for l in range(j, j + 3):
                if a[k][l] == d:
                    return False
        return True
    m = find_empty_cell()
    if not m:
        return True
    f, g = m
    for n in range(1, 10):
        if is_valid(n, (f, g)):
            a[f][g] = n
            if p97_sudoku_solver(a):
                return True
            a[f][g] = 0
    return False

def p98_nonogram_solver(a, b):
    c = len(a)
    d = len(b)
    e = {}
    def get_row_possibilities(f, g):
        h = tuple(f)
        if (h, g) in e:
            return e[(h, g)]
        if not f:
            return [[0] * g]
        i = []
        j = f[0]
        k = f[1:]
        l = sum(k) + len(k)
        for m in range(g - l - j + 1):
            if m + j <= g:
                n = [0] * m + [1] * j
                if k:
                    n += [0]
                o = g - len(n)
                p = get_row_possibilities(k, o)
                for q in p:
                    i.append(n + q)
        e[(h, g)] = i
        return i
    r = [get_row_possibilities(s, d) for s in a]
    t = [[0 for _ in range(d)] for _ in range(c)]
    def solve(u):
        if u == c:
            for v in range(d):
                w = [t[x][v] for x in range(c)]
                if tuple(filter(None, [len(list(z)) for y, z in groupby(w) if y == 1])) != tuple(b[v]):
                    return False
            return True
        for A in r[u]:
            t[u] = A
            B = True
            for C in range(d):
                D = [t[E][C] for E in range(u + 1)]
            if B:
                if solve(u + 1):
                    return True
        return False
    from itertools import groupby
    if solve(0):
        return t
    else:
        return None