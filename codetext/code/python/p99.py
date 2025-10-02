
import itertools

def p99_crossword_solver(a, b):
    c, d = _parse_puzzle_to_graph(b)
    if not c:
        return None
    e = _solve_backtrack(a, c)
    if e:
        return _render_solution(e, d)
    return None

def _parse_puzzle_to_graph(a):
    b = [c.rstrip() for c in a.strip().split('\n')]
    if not b:
        return None, (0, 0)
    d = len(b)
    e = max(len(c) for c in b) if b else 0
    f = [list(c.ljust(e)) for c in b]
    g = []
    for h in range(d):
        for i in range(e):
            if f[h][i] != ' ' and (i == 0 or f[h][i-1] == ' '):
                j = 0
                while i + j < e and f[h][i+j] != ' ':
                    j += 1
                if j > 1:
                    g.append(((h, i), (0, 1), j))
    for i in range(e):
        for h in range(d):
            if f[h][i] != ' ' and (h == 0 or f[h-1][i] == ' '):
                j = 0
                while h + j < d and f[h+j][i] != ' ':
                    j += 1
                if j > 1:
                    g.append(((h, i), (1, 0), j))
    k = {l: [] for l in g}
    m = [n for n in g if n[1] == (0, 1)]
    o = [n for n in g if n[1] == (1, 0)]
    for p in m:
        for q in o:
            r, s = p[0]
            t = p[2]
            u, v = q[0]
            w = q[2]
            if (r >= u and r < u + w and v >= s and v < s + t):
                x = (r, v)
                y = x[1] - s
                z = x[0] - u
                k[p].append((q, y, z))
                k[q].append((p, z, y))
    return k, (d, e)

def _solve_backtrack(a, b):
    c = sorted(list(b.keys()))
    d = set(a)
    e = [({}, d)]
    while e:
        f, g = e.pop()
        if len(f) == len(c):
            return f
        h = c[len(f)]
        i = h[2]
        j = [k for k in g if len(k) == i]
        for l in j:
            m = True
            for n, o, p in b[h]:
                if n in f:
                    q = f[n]
                    if l[o] != q[p]:
                        m = False
                        break
            if m:
                r = f.copy()
                r[h] = l
                s = g - {l}
                e.append((r, s))
    return None

def _render_solution(a, b):
    c, d = b
    e = [[' ' for _ in range(d)] for _ in range(c)]
    for f, g in a.items():
        (h, i), (j, k), l = f
        for m in range(l):
            e[h + m * j][i + m * k] = g[m]
    return "\n".join("".join(n) for n in e)

if __name__ == '__main__':
    words = ["ALPHA", "BRAVO", "CHARLIE", "DELTA", "ECHO", "FOXTROT", 
             "GOLF", "HOTEL", "INDIA", "JULIET", "KILO", "LIMA"]
    puzzle = """
..GOLF.
ALPHA..
.O.L...
.T.I...
.E.L...
BRAVO..
..O....
"""
    words_2 = ["AST", "LAS", "HAS", "SAH", "LET", "HAT"]
    puzzle_2 = """
...
. .
...
"""
    words_3 = ["A", "B"]
    puzzle_3 = """
. .
"""
    print("--- Puzzle 1 ---")
    solution = p99_crossword_solver(words, puzzle)
    if solution:
        print(solution)
    else:
        print("No solution found.")
    print("\n--- Puzzle 2 ---")
    puzzle_2_defined = """
. . .
 ... 
. . .
"""
    words_2_defined = ["ATE", "SEA", "SAT", "EAT"]
    solution_2 = p99_crossword_solver(words_2_defined, puzzle_2_defined)
    if solution_2:
        print(solution_2)
    else:
        print("No solution found.")
    print("\n--- Puzzle 3 (No Solution) ---")
    solution_3 = p99_crossword_solver(words_3, puzzle_3)
    if solution_3:
        print(solution_3)
    else:
        print("No solution found.")