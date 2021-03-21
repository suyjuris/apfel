import sys
import math

N = 10000000
m = 10 # slots per bucket
f = 0.75 # fill rate
n = 2**32 # number of possible hash values used to distinguish elements in bucket

slots = N/f/m

def binom(n, k):
    if k < 0 or k > n:
        return 0

    if k == 0 or k == n:
        return 1

    total_ways = 1
    for i in range(min(k, n - k)):
        total_ways = total_ways * (n - i) // (i + 1)

    return total_ways

def collision(bits, elements):
    p = 0
    N = 2**bits
    for i in range(elements): p += math.log(N - i, 2)
    p -= bits * elements
    return 1 - 2**p;

prob = 0
prob2 = 0 # probability that there is a collision in slot 1
p_notfull = 0
for i in range(4*m+1):
    p = 1/slots
    pp = binom(N, i) * p**i * (1-p) ** (N-i) # probability that there are exactly i items in slot 1
    prob += pp
    if i <= m: p_notfull += pp
    
    qq = 1 # probability that there is NO collision among the i items
    for j in range(i): qq *= (n-j) / n
    prob2 += (1 - qq) * pp
    
    print('%2d %.8f %.8f %.8f' % (i, pp, min((1 - prob)*slots, 1), 1 - qq))

    
print(slots, prob2, prob2 * slots, 1-(1-prob2)**slots)
print(1-p_notfull)
