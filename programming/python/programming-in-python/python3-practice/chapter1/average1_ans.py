#!/usr/bin/env python3



numbers = []
count=0
_sum=0

while True:
    try:
        line = input("enter a number or Enter to finish: ")
        if not line:
            break
        number = int(line)
        numbers += [number]
        count += 1
    except ValueError as err:
        print(err)


if numbers:
    print(numbers)
    for i in numbers:
        _sum += i
    
    print("count = ",count,"sum = ",_sum,
