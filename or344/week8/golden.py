import numpy as np
import math
import xlsxwriter as xw

def f(x, function):
    return eval(function)

def genx1(a, b):
    return b - 0.618034*(b-a)

def genx2(a, b):
    return a + 0.618034*(b-a)

def write_title(sheet):
    sheet.write("A1", "step")
    sheet.write("B1", "a")
    sheet.write("C1", "x1")
    sheet.write("D1", "x2")
    sheet.write("E1", "b")
    sheet.write("F1", "f1")
    sheet.write("G1", "f2")

def write_line(step, a, x1, x2, b, f1, f2, sheet):
    sheet.write(step-1, 0, str(step))
    sheet.write(step-1, 1, "%.4f"%(a))
    sheet.write(step-1, 2, "%.4f"%(x1))
    sheet.write(step-1, 3, "%.4f"%(x2))
    sheet.write(step-1, 4, "%.4f"%(b))
    sheet.write(step-1, 5, "%.4f"%(f1))
    sheet.write(step-1, 6, "%.4f"%(f2))

def golden(min, step, a, x1, x2, b , function, steps, rou=None, sheet=None):
    # region of uncertitiny base case
    if rou != None and b-a < rou:
        return

    x1 = genx1(a, b)
    x2 = genx2(a, b)
    f1 = f(x1, function)
    f2 = f(x2, function)

    # output
    if step == 1:
        write_title(sheet)
        print( "%-6s %-10.4s %-10.4s %-10.4s %-10.4s %-10.4s %-10.4s"%("step", "a", "x1", "x2", "b", "f1", "f2") )
    write_line(step+1, a, x1, x2, b, f1, f2, sheet)
    print( "%-6d %-10.4f %-10.4f %-10.4f %-10.4f %-10.4f %-10.4f"%(step+1, a, x1, x2, b, f1, f2) )

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

    golden(min, step+1, a, x1, x2, b, function, steps, rou, sheet)

if __name__ == '__main__':
    book = xw.Workbook("out.xlsx")
    sheet = book.add_worksheet()
    golden(min=True, step=1, a=0, x1=0, x2=2, b=2, function='x**2 + 2*np.exp(-x)', steps=14, rou=0.02, sheet=sheet)
    book.close()