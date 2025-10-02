import collections
import heapq
from itertools import combinations, permutations

class Graph:
    def __init__(self, a, b, c=False):
        self.nodes = set(a)
        self.edges = set(tuple(e) for e in b)
        self.directed = c
        self._adjacency_list = None
        self._rebuild_adj_list()

    def _rebuild_adj_list(self):
        self._adjacency_list = {node: [] for node in self.nodes}
        for edge in self.edges:
            u, v, *_ = edge
            self.nodes.add(u)
            self.nodes.add(v)
            if u in self._adjacency_list:
                self._adjacency_list[u].append(v)
            else:
                self._adjacency_list[u] = [v]
            if not self.directed:
                if v in self._adjacency_list:
                    self._adjacency_list[v].append(u)
                else:
                    self._adjacency_list[v] = [u]
        for node in self.nodes:
            if node not in self._adjacency_list:
                self._adjacency_list[node] = []

    @property
    def adjacency_list(self):
        if self._adjacency_list is None:
            self._rebuild_adj_list()
        return self._adjacency_list

    def get_neighbors(self, a):
        return self.adjacency_list.get(a, [])

    def get_edge_weight(self, a, b):
        for c in self.edges:
            d, e, *f = c
            g = f[0] if f else 1
            if (d == a and e == b) or \
               (not self.directed and d == b and e == a):
                return g
        return None

    @classmethod
    def p80_from_adjacency_list(cls, a, b=False):
        c, d = set(), set()
        for e, f in a.items():
            c.add(e)
            for g in f:
                c.add(g)
                h = tuple(sorted((e, g))) if not b else (e, g)
                d.add(h)
        return cls(list(c), list(d), b)

def p81_find_paths(a, b, c):
    d = []
    e = collections.deque([(b, [b])])
    while e:
        f, g = e.popleft()
        if f == c:
            d.append(g)
            continue
        for h in sorted(a.get_neighbors(f)):
            if h not in g:
                e.append((h, g + [h]))
    return d

def p82_find_cycles(a, b):
    c = []
    for d in sorted(a.get_neighbors(b)):
        for e in p81_find_paths(a, d, b):
            if len(e) > 1 or e[0] == b:
                c.append([b] + e)
    return c

def p83_construct_spanning_trees(a):
    b = len(a.nodes)
    if b == 0:
        return []
    c = []
    for d in combinations(a.edges, b - 1):
        e = Graph(a.nodes, d)
        if len(e.nodes) == b and p85_is_connected(e):
            c.append(e)
    return c

def p84_isomorphism(a, b):
    if len(a.nodes) != len(b.nodes) or len(a.edges) != len(b.edges):
        return False
    c = sorted(len(a.get_neighbors(n)) for n in a.nodes)
    d = sorted(len(b.get_neighbors(n)) for n in b.nodes)
    if c != d:
        return False
    e = list(a.nodes)
    for f in permutations(b.nodes):
        g = {e[i]: f[i] for i in range(len(e))}
        h = True
        for i, j, *_ in a.edges:
            if not b.get_edge_weight(g[i], g[j]):
                h = False
                break
        if h:
            return True
    return False

def p85_is_connected(a):
    if not a.nodes:
        return True
    b = next(iter(a.nodes))
    c = collections.deque([b])
    d = {b}
    while c:
        e = c.popleft()
        for f in a.get_neighbors(e):
            if f not in d:
                d.add(f)
                c.append(f)
    return len(d) == len(a.nodes)

def p86_minimal_spanning_tree(a):
    if not a.nodes:
        return None
    b, c = set(), set()
    d = next(iter(a.nodes))
    e = []
    c.add(d)
    for f in a.get_neighbors(d):
        g = a.get_edge_weight(d, f)
        heapq.heappush(e, (g, d, f))
    while e and len(c) < len(a.nodes):
        g, h, i = heapq.heappop(e)
        if i not in c:
            c.add(i)
            b.add(tuple(sorted((h, i))) + (g,))
            for j in a.get_neighbors(i):
                if j not in c:
                    k = a.get_edge_weight(i, j)
                    heapq.heappush(e, (k, i, j))
    if len(c) == len(a.nodes):
        return Graph(a.nodes, b)
    return None

def p87_graph_coloring(a, b):
    c = {}
    d = sorted(list(a.nodes))
    def solve(e):
        if e == len(d):
            return True
        f = d[e]
        for g in range(1, b + 1):
            if all(c.get(n) != g for n in a.get_neighbors(f)):
                c[f] = g
                if solve(e + 1):
                    return True
                del c[f]
        return False
    return c if solve(0) else None

def p88_bipartite_check(a):
    if not a.nodes:
        return True
    b = {}
    for c in a.nodes:
        if c not in b:
            b[c] = 1
            d = collections.deque([c])
            while d:
                e = d.popleft()
                for f in a.get_neighbors(e):
                    if f not in b:
                        b[f] = -b[e]
                        d.append(f)
                    elif b[f] == b[e]:
                        return False
    return True

def p89_topological_sort(a):
    if not a.directed:
        raise ValueError("Topological sort is for directed graphs only.")
    b = {n: 0 for n in a.nodes}
    for c in a.nodes:
        for d in a.get_neighbors(c):
            b[d] += 1
    e = collections.deque([n for n, d in b.items() if d == 0])
    f = []
    while e:
        g = e.popleft()
        f.append(g)
        for h in sorted(a.get_neighbors(g)):
            b[h] -= 1
            if b[h] == 0:
                e.append(h)
    return f if len(f) == len(a.nodes) else None