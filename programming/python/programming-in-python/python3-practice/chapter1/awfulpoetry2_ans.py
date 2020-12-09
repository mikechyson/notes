#!/usr/bin/env python3

import random
import time
import sys

articles = ["the", "a"]
subjects = ["cat", "dog", "man", "woman"]
verbs = ["sang", "ran", "jumped"]
adverbs = ["loudly", "quietly", "well", "badly"]

structures = [[articles,subjects,verbs,adverbs],
              [articles,subjects,verbs]]

default = 5
if len(sys.argv) > 1:
    default = int(sys.argv[1])

count = 0
while count < default:
    line = ""
    for i in structures[random.randint(0,1)]:
        line += random.choice(i) + " "
    print(line)
    time.sleep(1)
    count += 1
