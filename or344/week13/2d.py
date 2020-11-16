import math

def func(x):
    f = x*(5*math.pi - x)
    return f

phi = 1.618034
inv_phi = 0.618034

#define region of uncertainty
a = 0
b = 20
err = 1/20
x1 = b - inv_phi * (b - a)
x2 = a + inv_phi * (b - a)
Ln = b-a
Kn = Ln*inv_phi

# print("Step 2 : a =", a, "; x1 =", x1, "; x2 =", x2, "; b =", b, "; f(x1) =", func(x1), "; f(x2) =", func(x2))
print(2,";", a, ";", b, ";", Ln, ";", Kn, ";", x1, ";", x2, ";", func(x1), ";", func(x2))


i = 3
curr_err = 20
while curr_err > err:
    if func(x1) > func(x2): # < for minimise, would be opposite for max
        b = x2
        x2 = x1
        x1 = b - inv_phi * (b - a)
        curr_err = b - x1
    else:
        a = x1
        x1 = x2
        x2 = a + inv_phi * (b - a)
        curr_err = x2 - a

    Ln = b-a
    Kn = Ln*inv_phi

    #print("Step", i,  ": a =", a, "; x1 =", x1, "; x2 =", x2, "; b =", b, "; f(x1) =", func(x1), "; f(x2) =", func(x2))
    print(i,";", a, ";", b, ";", Ln, ";", Kn, ";", x1, ";", x2, ";", func(x1), ";", func(x2))

    i+=1 