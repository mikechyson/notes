#!/usr/bin/env python
import time

def fib2(n):
    if n == 0:
        return 0
    f = []
    f.append(0)
    f.append(1)
    for i in range(2, n+1):
        tmp = f[i-1] + f[i-2]
        f.append(tmp)
    return f[n]


start = time.time()
print(fib2(35))
end = time.time()

gap = end - start
print('time consumed(s): {}'.format(gap))


