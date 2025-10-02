class Node:
    def __init__(self, a, b=None, c=None):
        self.value = a
        self.left = b
        self.right = c
        self.x = 0
        self.y = 0
        self.modifier = 0

    def __repr__(self):
        return f"Node({self.value}, x={self.x}, y={self.y})"

def _get_height(a):
    if not a:
        return 0
    return 1 + max(_get_height(a.left), _get_height(a.right))

def p64(a):
    b = 0
    def _in_order_layout(c, d):
        nonlocal b
        if not c:
            return
        _in_order_layout(c.left, d + 1)
        b += 1
        c.x = b
        c.y = d
        _in_order_layout(c.right, d + 1)
    if a:
        _in_order_layout(a, 1)
    return a

def p65(a):
    b = _get_height(a)
    def _get_initial_x(c, d, e):
        if not c:
            return 0
        if not c.left:
            return e
        return _get_initial_x(c.left, d - 1, e / 2) + e

    def _layout(c, d, e, f):
        if not c:
            return
        c.x = e
        c.y = d
        _layout(c.left, d + 1, e - f, f / 2)
        _layout(c.right, d + 1, e + f, f / 2)

    if not a:
        return None
    c = 2 ** (b - 2)
    d = _get_initial_x(a, b, c)
    _layout(a, 1, d, c)
    return a

def p66(a):
    def _simple_bottom_up(b, c):
        if not b:
            return
        b.y = c
        _simple_bottom_up(b.left, c + 1)
        _simple_bottom_up(b.right, c + 1)
        if b.left and b.right:
            d = _find_max_x(b.left)
            e = _find_min_x(b.right)
            f = d - e + 1
            if f > 0:
                _shift_tree(b.right, f)
            b.x = (b.left.x + b.right.x) / 2
        elif b.left:
            b.x = b.left.x + 0.5
        elif b.right:
            b.x = b.right.x - 0.5
        else:
            b.x = 0

    def _find_max_x(b):
        if not b: return float('-inf')
        return max(b.x, _find_max_x(b.left), _find_max_x(b.right))

    def _find_min_x(b):
        if not b: return float('inf')
        return min(b.x, _find_min_x(b.left), _find_min_x(b.right))

    def _shift_tree(b, c):
        if not b: return
        b.x += c
        _shift_tree(b.left, c)
        _shift_tree(b.right, c)

    _simple_bottom_up(a, 1)
    return a