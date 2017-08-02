from collections import Counter
from math import log

def prime_fac(x):
    """Returning a map factor -> exponent."""
    c = Counter()
    for i in xrange(2, x + 1):
        while x % i == 0:
            c[i] += 1
            x //= i
    return c

def zeroes(base, n):
    factor_cnts = prime_fac(base)
    cnts = Counter()

    # Find number of factors for the whole factorial.
    for f in factor_cnts:
        cnts[f] =sum(n // (f ** i) for i in xrange(1, int(log(n, f) + 1)))
    return min(cnts[k] // factor_cnts[k] for k in factor_cnts)