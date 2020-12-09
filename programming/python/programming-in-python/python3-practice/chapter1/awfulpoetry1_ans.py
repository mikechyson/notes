#!/usr/bin/env python3

import random
import time

articles = ["the", "a"]
subjects = ["cat", "dog", "man", "woman"]
verbs = ["sang", "ran", "jumped"]
adverbs = ["loudly", "quietly", "well", "badly"]

structures = [[articles,subjects,verbs,adverbs],
              [articles,subjects,verbs]]

for i in [1,2,3,4,5]:
    line = ""
    for i in structures[random.randint(0,1)]:
        line += random.choice(i) + " "
    print(line)
    time.sleep(1)
