class Node:
    def __init__(self, a, b=None, c=None):
        self.value = a
        self.left = b
        self.right = c

    def __repr__(self):
        return f"Node({self.value})"

def p61(a):
    if not a:
        return []
    b = []
    c = [a]
    while c:
        d = c.pop()
        if d:
            if not d.left and not d.right:
                b.append(d.value)
            else:
                if d.right:
                    c.append(d.right)
                if d.left:
                    c.append(d.left)
    return b[::-1]

def p61a_count_leaves(a):
    return len(p61(a))

def p62(a):
    if not a:
        return []
    b = []
    c = [a]
    while c:
        d = c.pop(0)
        e = False
        if d.left:
            e = True
            c.append(d.left)
        if d.right:
            e = True
            c.append(d.right)
        if e:
            b.append(d.value)
    return b

def p62b_nodes_at_level(a, b):
    if not a or b < 1:
        return []
    if b == 1:
        return [a.value]
    c = p62b_nodes_at_level(a.left, b - 1)
    d = p62b_nodes_at_level(a.right, b - 1)
    return c + d

def p63(a, b='x'):
    if a <= 0:
        return None
    c = [Node(b) for _ in range(a)]
    for i in range(a):
        d = 2 * i + 1
        e = 2 * i + 2
        if d < a:
            c[i].left = c[d]
        if e < a:
            c[i].right = c[e]
    return c[0]