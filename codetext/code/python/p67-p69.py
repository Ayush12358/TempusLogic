class Node:
    def __init__(self, a, b=None, c=None):
        self.value = a
        self.left = b
        self.right = c

    def __repr__(self):
        return f"Node({self.value})"

def p67_tree_to_string(a):
    if not a:
        return ""
    if not a.left and not a.right:
        return str(a.value)
    b = p67_tree_to_string(a.left)
    c = p67_tree_to_string(a.right)
    return f"{a.value}({b},{c})"

def p67_string_to_tree(a):
    if not a:
        return None
    b = a.find('(')
    if b == -1:
        return Node(a)
    c = a[:b]
    d = a[b+1:-1]
    e = 0
    f = -1
    for g, h in enumerate(d):
        if h == '(':
            e += 1
        elif h == ')':
            e -= 1
        elif h == ',' and e == 0:
            f = g
            break
    if f == -1:
        return None
    i = d[:f]
    j = d[f+1:]
    k = p67_string_to_tree(i)
    l = p67_string_to_tree(j)
    return Node(c, k, l)

def p68_preorder_inorder_to_tree(a, b):
    if not a or not b:
        return None
    c = a[0]
    d = Node(c)
    try:
        e = b.index(c)
    except ValueError:
        return None
    f = b[:e]
    g = b[e+1:]
    h = len(f)
    i = a[1:1 + h]
    j = a[1 + h:]
    d.left = p68_preorder_inorder_to_tree(i, f)
    d.right = p68_preorder_inorder_to_tree(j, g)
    return d

def p69_tree_to_dotstring(a):
    if not a:
        return "."
    b = str(a.value)
    b += p69_tree_to_dotstring(a.left)
    b += p69_tree_to_dotstring(a.right)
    return b

def p69_dotstring_to_tree(a):
    b = iter(a)
    def _build():
        try:
            c = next(b)
            if c == '.':
                return None
            else:
                d = c
                e = _build()
                f = _build()
                return Node(d, e, f)
        except StopIteration:
            return None
    return _build()