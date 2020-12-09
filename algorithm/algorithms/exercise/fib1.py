#!/usr/bin/env python
import time

def fib1(n):
    if n == 0:
        return 0
    if n == 1:
        return 1
    return fib1(n-1) + fib1(n-2)

start = time.time()
print(fib1(35))
end = time.time()

gap = end - start
print('time consumed(s): {}'.format(gap))
