from itertools import combinations

def p94_generate_k_regular(a, b, c, d):
    if (a * b) % 2 != 0:
        return []
    e = list(range(1, a + 1))
    f = []
    g = {}
    def get_degree(h, i):
        return sum(1 for j in i if h in j)
    def solve(k, l):
        m = (tuple(sorted(k)), tuple(sorted(map(tuple, map(sorted, l)))))
        if m in g:
            return
        if not k:
            n = c(e, l)
            o = True
            for p in f:
                if d(n, p):
                    o = False
                    break
            if o:
                f.append(n)
            return
        q = k[0]
        r = k[1:]
        s = b - get_degree(q, l)
        if s < 0:
            return
        t = [u for u in r if get_degree(u, l) < b]
        if len(t) < s:
            return
        for v in combinations(t, s):
            w = l.copy()
            for x in v:
                w.add(tuple(sorted((q, x))))
            solve(r, w)
        g[m] = True
    solve(e, set())
    return f