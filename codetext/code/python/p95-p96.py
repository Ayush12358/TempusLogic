def p95_full_number_to_words(a):
    if a == 0:
        return "zero"
    b = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    c = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
    d = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
    e = ["", "thousand", "million", "billion", "trillion", "quadrillion", "quintillion", "sextillion", "septillion", "octillion", "nonillion"]
    def convert_less_than_thousand(f):
        if f == 0:
            return ""
        if f < 10:
            return b[f]
        if f < 20:
            return c[f - 10]
        if f < 100:
            g, h = divmod(f, 10)
            return d[g] + ("-" + b[h] if h > 0 else "")
        if f < 1000:
            i, j = divmod(f, 100)
            return b[i] + " hundred" + (" " + convert_less_than_thousand(j) if j > 0 else "")
    if a < 0:
        return "minus " + p95_full_number_to_words(abs(a))
    k = []
    l = 0
    while a > 0:
        if a % 1000 != 0:
            m = convert_less_than_thousand(a % 1000)
            k.append(m + (" " + e[l] if l > 0 else ""))
        a //= 1000
        l += 1
    return " ".join(reversed(k)).strip()

def p96_identifier_dfa(a):
    if not a:
        return False
    b = 0
    for c, d in enumerate(a):
        if b == 0:
            if d.isalpha():
                b = 1
            else:
                return False
        elif b == 1:
            if d.isalnum():
                pass
            elif d == '_':
                b = 2
            else:
                return False
        elif b == 2:
            if d.isalnum():
                b = 1
            else:
                return False
    return b == 1