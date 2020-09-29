import math

def f(x, function):
    return eval(function)

def genx4(x1, x2, x3, f1, f2, f3):
    return 0.5 * ( ((x2**2 - x3**2)*f1 + (x3**2 - x1**2)*f2 + (x1**2 - x2**2)*f3) / ((x2 - x3)*f1 + (x3 - x1)*f2 + (x1 - x2)*f3))

def powell(x1, x2, x3, step, steps, function, rou=None):
    # region of uncertinty reached
    if rou and x3-x1 <= rou:
        return

    f1 = f(x1, function)
    f2 = f(x2, function)
    f3 = f(x3, function)
    x4 = genx4(x1, x2, x3, f1, f2, f3)
    f4 = f(x4, function)

    # output
    if step == 1:
        print( "%-6s %-10s %-10s %-10s %-10s %-10s %-10s %-10s %-10s"%("step", "x1", "x2", "x3", "f1", "f2", "f3", "x4", "f4") )
    print( "%-6d %-10.4f %-10.4f %-10.4f %-10.4f %-10.4f %-10.4f %-10.4f %-10.4f"%(step, x1, x2, x3, f1, f2, f3, x4, f4) )

    #! missing max case
    # changing (x1, x2 x3)
    x = [x1, x2, x3, x4]
    if x2 < x4 and x4 < x3:
        if f4 < f2:
            x1 = x[1]
            x2 = x[3]
            x3 = x[2]
        elif f2 < f4 and f4 <= f3:
            x1 = x[0]
            x2 = x[1]
            x3 = x[3]
        else:
            print('this case has not been accounted for(0)')
    elif x1 < x4 and x4 < x2:
        if f4 < f2:
            x1 = x[0]
            x2 = x[3]
            x3 = x[1]
        elif f2 < f4 and f4 <= f1:
            x1 = x[3]
            x2 = x[1]
            x3 = x[2]
        else:
            print('this case has not been accounted for(2)')
    else:
        print('this case has not been accounted for(3)')

    # base case
    if step < steps:
        powell(x1, x2, x3, step+1, steps, function, rou)

if __name__ == '__main__':
    powell(x1=1, x2=3.75, x3=6.5, step=1, steps=7, function='(3-4*x)/(1+x**2)', rou=None)