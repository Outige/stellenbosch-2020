import xlsxwriter as xw
import math

def write_title(sheet):
    sheet.write("A1", "x1")
    sheet.write("B1", "x2")
    sheet.write("C1", "x3")
    sheet.write("D1", "f1")
    sheet.write("E1", "f2")
    sheet.write("F1", "f3")
    sheet.write("G1", "x4")
    sheet.write("H1", "f4")

def write_line(step, x1, x2, x3, f1, f2, f3, x4, f4, sheet):
    sheet.write(step, 0, "%.4f"%(x1))
    sheet.write(step, 1, "%.4f"%(x2))
    sheet.write(step, 2, "%.4f"%(x3))
    sheet.write(step, 3, "%.4f"%(f1))
    sheet.write(step, 4, "%.4f"%(f2))
    sheet.write(step, 5, "%.4f"%(f3))
    sheet.write(step, 6, "%.4f"%(x4))
    sheet.write(step, 7, "%.4f"%(f4))

def f(x, function):
    return eval(function)

def genx4(x1, x2, x3, f1, f2, f3):
    return 0.5 * ( ((x2**2 - x3**2)*f1 + (x3**2 - x1**2)*f2 + (x1**2 - x2**2)*f3) / ((x2 - x3)*f1 + (x3 - x1)*f2 + (x1 - x2)*f3))

def powell(x1, x2, x3, step, steps, function, min, rou=None, sheet=None):
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
        write_title(sheet)
    write_line(step, x1, x2, x3, f1, f2, f3, x4, f4, sheet)
    print( "%-6d %-10.4f %-10.4f %-10.4f %-10.4f %-10.4f %-10.4f %-10.4f %-10.4f"%(step, x1, x2, x3, f1, f2, f3, x4, f4) )

    #! missing max case
    # changing (x1, x2 x3)
    x = [x1, x2, x3, x4]
    if min:
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
    else:
        print("oof")
        return

    # base case
    if step < steps:
        powell(x1, x2, x3, step+1, steps, function, min, rou, sheet)

if __name__ == '__main__':
    book = xw.Workbook("out.xlsx")
    sheet = book.add_worksheet()
    powell(x1=1, x2=3.75, x3=6.5, step=1, steps=7, function='(3-4*x)/(1+x**2)', min=True, rou=None, sheet=sheet)
    book.close()