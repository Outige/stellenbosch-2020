import numpy as np
import math

def f(x, function):
    return eval(function)

def genx1(a, b):
    return b - 0.618034*(b-a)

def genx2(a, b):
    return a + 0.618034*(b-a)

def golden(min, step, a, x1, x2, b , function, steps, rou=None):
    # region of uncertitiny base case
    if rou != None and b-a < rou:
        return

    x1 = genx1(a, b)
    x2 = genx2(a, b)
    f1 = f(x1, function)
    f2 = f(x2, function)

    # output
    if step == 1:
        print( "%-6s %-10.4s %-10.4s %-10.4s %-10.4s %-10.4s %-10.4s"%("step", "a", "x1", "x2", "b", "f1", "f2") )
    print( "%-6d %-10.4f %-10.4f %-10.4f %-10.4f %-10.4f %-10.4f"%(step, a, x1, x2, b, f1, f2) )

    # base case
    if step == steps:
        return
    
    # changing a,b
    if min:
        if f2 > f1:
            a = a
            b = x2
        elif f2 < f1:
            a = x1
            b = b
        elif f2 == f1:
            a = x1
            b = x2
        else:
            print('this case has not been accounted for(0)')
    else:
        if f2 < f1:
            a = a
            b = x2
        elif f2 > f1:
            a = x1
            b = b
        elif f2 == f1:
            a = x1
            b = x2
        else:
            print('this case has not been accounted for(1)')

    golden(min, step+1, a, x1, x2, b, function, steps, rou)

if __name__ == '__main__':
    golden(min=False, step=1, a=0, x1=0, x2=20, b=20, function='x*(5*math.pi -x)', steps=14, rou=1)