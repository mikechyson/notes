import cmath
import math
import sys


# important function thought
def get_float(msg, allow_zero):
    x = None
    while x is None:
        try:
            x = float(input(msg))
            if not allow_zero and abs(x) < sys.float_info.epsilon:
                print('zero is not allowed')
                x = None
        except ValueError as err:
            print(err)
    return x


a = get_float("enter a: ", False)
b = get_float("enter b: ", True)
c = get_float("enter c: ", True)

# learn its logic deal
x1 = None
x2 = None
discriminant = b ** 2 - 4 * a * c
if discriminant == 0:
    x1 = -(b / (2 * a))
else:
    if discriminant > 0:
        root = math.sqrt(discriminant)
    else:
        root = cmath.sqrt(discriminant)
    x1 = (-b + root) / (2 * a)
    x2 = (-b - root) / (2 * a)

# learn its formatting
# equation = '{0}x\N{superscript two} + {1}x + {2} = 0 \N{rightwards arrow} x = {3}'.format(a,b,c,x1)
equation = '{a}x\N{superscript two} + {b}x + {c} = 0 \N{rightwards arrow} x = {x1}'.format(**locals())
if x2 is not None:
    equation += ' or x = {}'.format(x2)
print(equation)
