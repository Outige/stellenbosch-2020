import sys

def func(x):
    f = x*(5*3.1415 - x)
    f = x**2
    return f

def fib(n):
    if n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fib(n-1) + fib(n-2)


def run(max, accuracy, a, b):
    """accuracy = epsilon / abs(a - b)"""

    # accuracy = 0.05
    p = 1 / float(accuracy)
    n = 1

    while fib(n) < p:
        n = n + 1

    x1 = a + fib(n-2) / fib(n) * (b-a)
    x2 = a + fib(n-1) / fib(n) * (b-a)

    print("step, a, x1, x2, b, f(x1), f(x2)")
    print('{0}, {1:.5}, {2:.5}, {3:.5}, {4:.5}, {5:.5}, {6:.5}'.format(
        2, float(a), float(x1), float(x2), float(b), float(func(x1)), float(func(x2))))
    for i in range(3, n):
        n = n-1
        if(max):
            if func(x1) > func(x2):
                b = x2
                x2 = x1
                x1 = a + fib(n-2) / fib(n) * (b-a)
            else:
                a = x1
                x1 = x2
                x2 = a + fib(n-1) / fib(n) * (b-a)
        else:
            if func(x1) < func(x2):
                b = x2
                x2 = x1
                x1 = a + fib(n-2) / fib(n) * (b-a)
            else:
                a = x1
                x1 = x2
                x2 = a + fib(n-1) / fib(n) * (b-a)

        print('{0}, {1:.5}, {2:.5}, {3:.5}, {4:.5}, {5:.5}, {6:.5}'.format(
            i, float(a), float(x1), float(x2), float(b), float(func(x1)), float(func(x2))))
        i += 1

if __name__ == "__main__":

    max = True
    if sys.argv[1] != 'max':
        max = False
    accuracy = float(sys.argv[2])
    a = float(sys.argv[3])
    b = float(sys.argv[4])
    run(max, accuracy, a, b)