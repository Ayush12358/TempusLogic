import itertools

# p46, p47, p48 are all related to truth tables.
# p46 is the core function to generate a truth table.
# p47 and p48 are applications of p46.

def p46_table(variables, expression_func):
    """
    Prints a truth table for a given boolean expression function.

    :param variables: A list of variable names (strings).
    :param expression_func: A function that takes a list of booleans and returns a boolean.
    """
    
    # Print header
    header = " ".join(variables) + " | RESULT"
    print(header)
    print("-" * len(header))

    # Generate all possible boolean combinations for the variables
    num_vars = len(variables)
    for combination in itertools.product([True, False], repeat=num_vars):
        result = expression_func(combination)
        
        # Format the row for printing
        row_values = " ".join(str(val).ljust(len(var)) for var, val in zip(variables, combination))
        print(f"{row_values} | {result}")

# Boolean logic helper functions, translated from Lisp
def nand_op(a, b):
    return not (a and b)

def nor_op(a, b):
    return not (a or b)

def xor_op(a, b):
    return a != b

def impl_op(a, b):
    return (not a) or b

def equ_op(a, b):
    return a == b

# p47: Example usage for 'and' and 'or'
def p47():
    print("Truth table for AND:")
    p46_table(['A', 'B'], lambda v: v[0] and v[1])
    
    import itertools
import heapq

def p46_table(a, b):
    c = " ".join(a) + " | RESULT"
    print(c)
    print("-" * len(c))
    d = len(a)
    for e in itertools.product([True, False], repeat=d):
        f = b(e)
        g = " ".join(str(h).ljust(len(i)) for i, h in zip(a, e))
        print(f"{g} | {f}")

def nand_op(a, b):
    return not (a and b)

def nor_op(a, b):
    return not (a or b)

def xor_op(a, b):
    return a != b

def impl_op(a, b):
    return (not a) or b

def equ_op(a, b):
    return a == b

def p47():
    print("Truth table for AND:")
    p46_table(['A', 'B'], lambda v: v[0] and v[1])
    print("\nTruth table for OR:")
    p46_table(['A', 'B'], lambda v: v[0] or v[1])

def p48():
    print("\nTruth table for (A and (B or C)) equ (A and B or A and C):")
    a = ['A', 'B', 'C']
    b = lambda v: equ_op((v[0] and (v[1] or v[2])), ((v[0] and v[1]) or (v[0] and v[2])))
    p46_table(a, b)

_gray_code_cache = {}

def p49(a):
    if a <= 0:
        return []
    if a in _gray_code_cache:
        return _gray_code_cache[a]
    if a == 1:
        b = ['0', '1']
        _gray_code_cache[1] = b
        return b
    c = p49(a - 1)
    b = []
    for d in c:
        b.append('0' + d)
    for d in reversed(c):
        b.append('1' + d)
    _gray_code_cache[a] = b
    return b

def p50(a):
    if not a:
        return []
    b = [[c, [d, ""]] for d, c in a]
    heapq.heapify(b)
    while len(b) > 1:
        d = heapq.heappop(b)
        e = heapq.heappop(b)
        for f in d[1:]:
            f[1] = '0' + f[1]
        for f in e[1:]:
            f[1] = '1' + f[1]
        g = [d[0] + e[0]] + d[1:] + e[1:]
        heapq.heappush(b, g)
    h = sorted(heapq.heappop(b)[1:], key=lambda x: (len(x[-1]), x[0]))
    return h
    p46_table(['A', 'B'], lambda v: v[0] or v[1])

# p48: Example for a more complex expression
def p48():
    print("\nTruth table for (A and (B or C)) equ (A and B or A and C):")
    variables = ['A', 'B', 'C']
    expression = lambda v: equ_op( (v[0] and (v[1] or v[2])), ((v[0] and v[1]) or (v[0] and v[2])) )
    p46_table(variables, expression)


# --- Huffman Coding ---

import heapq

# Using a simple dictionary for memoization, similar to the Lisp `let` block.
_gray_code_cache = {}

def p49(n):
    """
    Generates Gray codes of length n.
    Gray codes are sequences where consecutive codes differ by only one bit.
    """
    if n <= 0:
        return []
    if n in _gray_code_cache:
        return _gray_code_cache[n]
    
    if n == 1:
        result = ['0', '1']
        _gray_code_cache[1] = result
        return result

    # Recursive step
    prev_gray_codes = p49(n - 1)
    
    # Imperative style build-up
    result = []
    # Prepend '0' to the forward list
    for code in prev_gray_codes:
        result.append('0' + code)
    # Prepend '1' to the reversed list
    for code in reversed(prev_gray_codes):
        result.append('1' + code)
        
    _gray_code_cache[n] = result
    return result

def p50(freq_data):
    """
    Generates Huffman codes for a given set of symbols and their frequencies.
    
    :param freq_data: A list of (symbol, frequency) tuples.
    :return: A dictionary mapping symbols to their Huffman codes.
    """
    if not freq_data:
        return {}

    # A min-heap (priority queue) is perfect for building the Huffman tree.
    # Each item in the heap is a tuple: (frequency, symbol_or_tree).
    heap = [[freq, [symbol, ""]] for symbol, freq in freq_data]
    heapq.heapify(heap)

    # If there's only one symbol, its code is '0' or '1'. Conventionally '0'.
    if len(heap) == 1:
        return {heap[0][1][0]: '0'}

    # Build the Huffman tree by repeatedly combining the two lowest frequency nodes.
    while len(heap) > 1:
        # Pop the two nodes with the smallest frequencies
        lo = heapq.heappop(heap)
        hi = heapq.heappop(heap)
        
        # Assign codes: 0 for the left branch (lo), 1 for the right branch (hi)
        for pair in lo[1:]:
            pair[1] = '0' + pair[1]
        for pair in hi[1:]:
            pair[1] = '1' + pair[1]
            
        # Combine the nodes and push the new node back into the heap
        new_node = [lo[0] + hi[0]] + lo[1:] + hi[1:]
        heapq.heappush(heap, new_node)

    # The final heap contains the root of the tree.
    # The codes have been built up during the process.
    huffman_tree = heap[0]
    
    # Create the final code dictionary
    huffman_codes = {symbol: code for symbol, code in huffman_tree[1:]}
    
    return huffman_codes

