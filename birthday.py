import sys
import math

N = 10000000
m = 12 # slots per bucket
f = 0.7 # fill rate

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


prob = 0
for i in range(4*m+1):
    p = 1/slots
    prob += binom(N, i) * p**i * (1-p) ** (N-i)
    print('%2d %.8f' % (i, min((1 - prob)*slots, 1)))
    
n = 2**32
prob = 1
for i in range(32):
    prob *= (n-i-1) / (n-i)
print(prob, prob**slots)
