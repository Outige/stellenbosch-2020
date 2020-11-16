import numpy as np
import sympy as sym

x,y,z = sym.symbols('x y z')
func = ((5 * x) ** 2) - (2 * x * y) + 4 * (y ** 2) - (2 * y * z) + (6 * (z ** 2))
print("Function to minimise", func)

x0 = np.array([4, 1, 3])
eps = 0.1

def grad_vec(a, b, c):
    grad_x = sym.diff(func, x)
    grad_y = sym.diff(func, y)
    grad_z = sym.diff(func, z)
    vec = [grad_x, grad_y, grad_z]
    vec_eval = [grad_x.subs([(x, a), (y, b), (z, c)]), grad_y.subs([(x, a), (y, b), (z, c)]), grad_z.subs([(x, a), (y, b), (z, c)])]

    vec_mag = sym.N(np.sqrt(float(vec_eval[0] ** 2 + vec_eval[1] ** 2 + vec_eval[2] ** 2)), n=4)
    vec_mag = float(vec_mag)

    return vec, vec_eval, vec_mag

print("Gradient vector: ", grad_vec(x0[0], x0[1], x0[2])[0])
print("Gradient vector at x0 = ", grad_vec(x0[0], x0[1], x0[2])[1], "with size = ", grad_vec(x0[0], x0[1], x0[2])[2])


while grad_vec(x0[0],x0[1],x0[2])[2] >= eps:

    print("new x_k", x0)
    lam = sym.symbols('lam')

    min_func = func.subs([(x, (x0[0] - grad_vec(x0[0],x0[1],x0[2])[1][0] * lam)), (y, (x0[1] - grad_vec(x0[0],x0[1], x0[2])[1][1] * lam )), (z, (x0[2] - grad_vec(x0[0],x0[1], x0[2])[1][2] * lam ))])
    grad_lam = sym.diff(min_func, lam)
    min_lam = sym.N(sym.solve(grad_lam, lam)[0])
    print("lambda min = ", min_lam)
    x_n = np.array([(x0[0] - min_lam * grad_vec(x0[0],x0[1],x0[2])[1][0]), (x0[1] - min_lam * grad_vec(x0[0],x0[1],x0[2])[1][1]), (x0[2] - min_lam * grad_vec(x0[0],x0[1],x0[2])[1][2])])
    print(x_n)
    x0 = x_n 