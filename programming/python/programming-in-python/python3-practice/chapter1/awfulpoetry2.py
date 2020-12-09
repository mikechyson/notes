#!/usr/bin/env python3

import random
import time

articles = ["the", "a"]
subjects = ["cat", "dog", "man", "woman"]
verbs = ["sang", "ran", "jumped"]
adverbs = ["loudly", "quietly", "well", "badly"]

structures = [[articles,subjects,verbs,adverbs],
              [articles,subjects,verbs]]


def get_int(msg,minimum,maximum,default):
    while True:
        try:
            line = input(msg)
            if not line:
                return default
            number = int(line)
            if minimum<= number <= maximum:
                return number
            continue
        except ValueError as err:
            print(err)

count = 0
number = get_int("input a number between 1 and 10: ",1,10,5)
while count<number:
    line = ""
    for i in structures[random.randint(0,1)]:
        line += random.choice(i) + " "
    print(line)
    time.sleep(1)
    count += 1

