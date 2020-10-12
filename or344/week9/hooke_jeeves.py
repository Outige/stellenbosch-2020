import numpy as np

def f(x, function):
    return eval(function)

def scout(b, function, end):
    steps = [
        [-1 + b[0], -1 + b[1]],
        [0 + b[0], -1 + b[1]],
        [1 + b[0], -1 + b[1]],
        [-1 + b[0], 0 + b[1]],
        [0 + b[0], 0 + b[1]],
        [1 + b[0], 0 + b[1]],
        [-1 + b[0], 1 + b[1]],
        [0 + b[0], 1 + b[1]],
        [1 + b[0], 1 + b[1]]
    ]
    fmin = f(b, function)
    for step in steps:
        fstep = f(step, function)
        if fstep < fmin:
            fmin = fstep
            b = step
    print(b, fmin)
    if end > 0:
        scout(b, function, end-1)

def hooke_jeeves(b, function):
    scout(b, function, 4)

if __name__ == '__main__':
    #! `-`should be `+`?
    function = '3*x[0]**2 - 2*x[0]*x[1] + x[1]**2 + 4*x[0] + 3*x[1]'
    hooke_jeeves([0, 0], function)
    # print([0, 0], f([0, 0], function))
    # print([1, 0], f([1, 0], function))
    # print([-1, 0], f([-1, 0], function))
    # print([-1, 1], f([-1, 1], function))
    # print([-1, -1], f([-1, -1], function))