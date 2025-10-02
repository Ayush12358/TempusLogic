# In Python, a multiway tree can be represented with a Node class
# where each node has a value and a list of children.

class MTree:
    def __init__(self, a, b=None):
        self.value = a
        self.children = b if b is not None else []

    def __repr__(self):
        return f"MTree({self.value}, {self.children})"

def p70b_tree_to_lisp_string(a):
    if not a:
        return ""
    b = []
    for c in a.children:
        b.append(p70b_tree_to_lisp_string(c))
    d = " ".join(b)
    return f"({a.value} {d})"

def p70c_count_nodes(a):
    if not a:
        return 0
    b = 1
    c = list(a.children)
    while c:
        d = c.pop()
        b += 1
        for e in d.children:
            c.append(e)
    return b

def p71_internal_path_length(a):
    if not a:
        return 0
    b = 0
    c = [(a, 0)]
    while c:
        d, e = c.pop(0)
        b += e
        for f in d.children:
            c.append((f, e + 1))
    return b

def p72_postorder_traversal(a):
    if not a:
        return []
    b = []
    def _traverse(c):
        for d in c.children:
            _traverse(d)
        b.append(c.value)
    _traverse(a)
    return b

def p73_tree_to_prolog_string(a):
    if not a:
        return ""
    if not a.children:
        return str(a.value)
    b = []
    for c in a.children:
        b.append(p73_tree_to_prolog_string(c))
    d = ",".join(b)
    return f"{a.value}({d})"
