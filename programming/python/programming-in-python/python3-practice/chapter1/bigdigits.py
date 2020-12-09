#!/usr/bin/env python3

# Given a number on the command line, the program outputs the same number onto the console using "big digits"

import sys
import time

# Python statements normally occupy a single line,
# but they can span multiple lines if they are
# a parenthesized expression, a list, set, or dictrionary literal,
# a function call argument list, or a multiline statement
# where every end-of-line character except the last is escaped by
# preceding it with a backslash (\).
# In all these cases any number of lines can be spanned and
# indentation does not matter for the second and subsequent lines.
Zero=["  ***  ",
      " *   * ",
      "*     *",
      "*     *",
      "*     *",
      " *   * ",
      "  ***  "]

One=[" * ",
     "** ",
     " * ",
     " * ",
     " * ",
     " * ",
     "***"]

Two=[" *** ",
     "*   *",
     "*  * ",
     "  *  ",
     " *   ",
     "*    ",
     "*****"]

Three=[" *** ",
       "*   *",
       "    *",
       "  ** ",
       "    *",
       "*   *",
       " *** "]

Four=["   *  ",
      "  **  ",
      " * *  ",
      "*  *  ",
      "******",
      "   *  ",
      "   *  "]

Five=["**** ",
      "*    ",
      "**** ",
      "    *",
      "    *",
      "   * ",
      "***  "]

Six=[" *** ",
     "*    ",
     "*    ",
     "**** ",
     "*   *",
     "*   *",
     " *** "]

Seven=["*****",
       "    *",
       "   * ",
       "  *  ",
       " *   ",
       "*    ",
       "*    "]

Eight=[" *** ",
       "*   *",
       "*   *",
       " *** ",
       "*   *",
       "*   *",
       " *** "]

Nine=[" ****",
      "*   *",
      "*   *",
      " ****",
      "    *",
      "    *",
      "    *"]

Digits = [Zero, One, Two, Three, Four,Five, Six, Seven, Eight, Nine]

try:
    digits = sys.argv[1]
    row=0
    while row <7:               # lists have seven elements
        line=""
        column=0
        while column < len(digits):
            number = int(digits[column])
            digit = Digits[number]
            line += digit[row] + "   "
            column += 1
        print(line)
        row+=1
        time.sleep(1)
except IndexError:
    print("usage: bigdigits.py <number>")
except ValueError as err:
    print(err, "in", digits)


        
