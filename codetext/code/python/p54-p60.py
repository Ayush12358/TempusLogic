import itertools

class Node:
    def __init__(self, a, b=None, c=None):
        self.value = a
        self.left = b
        self.right = c

    def __repr__(self):
        return f"Node({self.value})"

    def to_list(self):
        a = [self.value]
        if self.left:
            a.append(self.left.to_list())
        else:
            a.append(None)
        if self.right:
            a.append(self.right.to_list())
        else:
            a.append(None)
        return a

def p54_is_tree(a):
    if not isinstance(a, list) or len(a) != 3:
        return False
    if a[1] is not None and not p54_is_tree(a[1]):
        return False
    if a[2] is not None and not p54_is_tree(a[2]):
        return False
    return True

def p55_cbal_trees(a, b='x'):
    if a == 0:
        return [None]
    if a == 1:
        return [Node(b)]
    if (a - 1) % 2 == 0:
        c = (a - 1) // 2
        d = p55_cbal_trees(c, b)
        e = p55_cbal_trees(c, b)
        f = []
        for g in d:
            for h in e:
                f.append(Node(b, g, h))
                if g is not h:
                    f.append(Node(b, h, g))
        return f
    else:
        c = (a - 1) // 2
        i = c + 1
        j = p55_cbal_trees(c, b)
        k = p55_cbal_trees(i, b)
        f = []
        for l in j:
            for m in k:
                f.append(Node(b, l, m))
                f.append(Node(b, m, l))
        return f

def p56_is_symmetric(a):
    if not a:
        return True
    def _is_mirror(b, c):
        if not b and not c:
            return True
        if not b or not c:
            return False
        return (b.value == c.value and
                _is_mirror(b.left, c.right) and
                _is_mirror(b.right, c.left))
    return _is_mirror(a.left, a.right)

def p57_construct_bst(a):
    if not a:
        return None
    b = Node(a[0])
    def _insert(c, d):
        if d < c.value:
            if c.left is None:
                c.left = Node(d)
            else:
                _insert(c.left, d)
        elif d > c.value:
            if c.right is None:
                c.right = Node(d)
            else:
                _insert(c.right, d)
    for e in a[1:]:
        _insert(b, e)
    return b

def p58_sym_cbal_trees(a, b='x'):
    c = p55_cbal_trees(a, b)
    d = []
    for e in c:
        if p56_is_symmetric(e):
            d.append(e)
    return d

_hbal_cache = {}

def _get_height(a):
    if not a:
        return 0
    return 1 + max(_get_height(a.left), _get_height(a.right))

def p59_hbal_trees(a, b='x'):
    if a in _hbal_cache:
        return _hbal_cache[a]
    if a == 0:
        return [None]
    if a == 1:
        return [Node(b)]
    c = p59_hbal_trees(a - 1, b)
    d = p59_hbal_trees(a - 2, b)
    e = []
    for f in c:
        for g in c:
            e.append(Node(b, f, g))
    for f in c:
        for h in d:
            e.append(Node(b, f, h))
            e.append(Node(b, h, f))
    _hbal_cache[a] = e
    return e

def p60_min_nodes_for_hbal(a):
    if a == 0:
        return 0
    if a == 1:
        return 1
    b = 0
    c = 1
    for _ in range(2, a + 1):
        d = c + b + 1
        b = c
        c = d
    return c

def p60_max_height_for_hbal(a):
    if a == 0:
        return 0
    b = 0
    c = 0
    while c <= a:
        b += 1
        c = p60_min_nodes_for_hbal(b)
    return b - 1

def p60_hbal_trees_with_nodes(a, b='x'):
    def _count_nodes(c):
        if not c:
            return 0
        return 1 + _count_nodes(c.left) + _count_nodes(c.right)
    c = 0
    h = 0
    while (2**(h+1) - 1) < a:
        h += 1
    d = h
    e = p60_max_height_for_hbal(a)
    f = []
    for g in range(d, e + 1):
        h = p59_hbal_trees(g, b)
        for i in h:
            if _count_nodes(i) == a:
                f.append(i)
    return f
