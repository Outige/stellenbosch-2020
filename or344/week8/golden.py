import numpy as np

def f(x, function):
    return eval(function)

def genx1(a, b):
    return b - 0.618034*(b-a)

def genx2(a, b):
    return a + 0.618034*(b-a)

def golden(step, a, x1, x2, b , function, steps, rou=None):
    # region of uncertitiny base case
    if rou != None and b-a < rou:
        return

    x1 = genx1(a, b)
    x2 = genx2(a, b)
    f1 = f(x1, function)
    f2 = f(x2, function)

    # output
    if step == 2:
        print( "%-6s %-10.4s %-10.4s %-10.4s %-10.4s %-10.4s %-10.4s"%("step", "a", "x1", "x2", "b", "f1", "f2") )
    print( "%-6d %-10.4f %-10.4f %-10.4f %-10.4f %-10.4f %-10.4f"%(step, a, x1, x2, b, f1, f2) )

    # base case
    if step == steps:
        return
    
    # changing a,b
    if f2 > f1:
        b = x2
    elif f2 < f1:
        a = x1
    else:
        print('this case has not been accounted for(0)')

    golden(step+1, a, x1, x2, b, function, steps, rou)

if __name__ == '__main__':
    golden(step=2, a=0, x1=-0.5, x2=0, b=2, function='x**2 + 2*np.exp(-x)', steps=14, rou=0.02)