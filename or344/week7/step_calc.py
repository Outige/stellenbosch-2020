import sys

def Fibonacci(n):
    if n<=0:
        print("Incorrect input")
    elif n==1:
        return 0
    elif n==2:
        return 1
    else:
        return Fibonacci(n-1)+Fibonacci(n-2)

def fibmin(x):
    for i in range(1, x):
        y = Fibonacci(i)
        if y >= x:
            print("fib(%d) = %d"%(i-2, y))
            return

if __name__ == '__main__':
    args = sys.argv[1:]
    if len(args) != 1:
        print("USAGE:\npython3 step_calc.py <some int>")
    else:
        fibmin(int(args[0]))