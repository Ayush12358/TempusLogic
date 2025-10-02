from itertools import permutations

def p92_graceful_labeling(a):
    b = len(a.nodes)
    if b == 0:
        return {}
    if len(a.edges) != b - 1:
        return None
    c = list(a.nodes)
    def solve(d, e, f):
        if len(d) == b:
            return d
        g = c[len(d)]
        for h in range(1, b + 1):
            if h not in e:
                i = True
                j = set()
                for k in a.get_neighbors(g):
                    if k in d:
                        l = abs(h - d[k])
                        if l == 0 or l in f or l in j:
                            i = False
                            break
                        j.add(l)
                if i:
                    d[g] = h
                    e.add(h)
                    m = solve(d, e, f.union(j))
                    if m:
                        return m
                    e.remove(h)
                    del d[g]
        return None
    return solve({}, set(), set())