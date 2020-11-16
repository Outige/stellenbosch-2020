import sys
import math
import xlsxwriter as xw

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

def func(x, function):
    # f = x * (5*math.pi - x)
    # return f
    return eval(function)

def fib(n):
    if n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fib(n-1) + fib(n-2)


def run(max, accuracy, a, b, function, sheet):
    """accuracy = epsilon / abs(a - b)"""

    # accuracy = 0.05
    p = 1 / float(accuracy)
    n = 1

    while fib(n) < p:
        n = n + 1

    x1 = a + fib(n-2) / fib(n) * (b-a)
    x2 = a + fib(n-1) / fib(n) * (b-a)

    write_title(sheet)
    print("step, a, x1, x2, b, f(x1), f(x2)")
    print('{0}, {1:.5}, {2:.5}, {3:.5}, {4:.5}, {5:.5}, {6:.5}'.format(
        2, float(a), float(x1), float(x2), float(b), float(func(x1, function)), float(func(x2, function))))
    write_line(2, a, x1, x2, b, func(x1, function), func(x2, function), sheet)
    for i in range(3, n):
        n = n-1
        if(max):
            if func(x1, function) > func(x2, function):
                b = x2
                x2 = x1
                x1 = a + fib(n-2) / fib(n) * (b-a)
            else:
                a = x1
                x1 = x2
                x2 = a + fib(n-1) / fib(n) * (b-a)
        else:
            if func(x1, function) < func(x2, function):
                b = x2
                x2 = x1
                x1 = a + fib(n-2) / fib(n) * (b-a)
            else:
                a = x1
                x1 = x2
                x2 = a + fib(n-1) / fib(n) * (b-a)

        print('{0}, {1:.5}, {2:.5}, {3:.5}, {4:.5}, {5:.5}, {6:.5}'.format(
            i, float(a), float(x1), float(x2), float(b), float(func(x1, function)), float(func(x2, function))))
        write_line(i, a, x1, x2, b, func(x1, function), func(x2, function), sheet)
        i += 1

if __name__ == "__main__":

    max = True

    '''
        1) given
        2) given epsilon. epsilon/(abs(b-a))
        3) do x many iterations: Look at look at what f(x) is, then set accuracy to 1/f(x)
    '''
    accuracy = 1/20
    a = 0
    b = 20
    function = "x * (5*math.pi - x)"

    book = xw.Workbook("out.xlsx")
    sheet = book.add_worksheet()
    run(max, accuracy, a, b, function, sheet)
    book.close()