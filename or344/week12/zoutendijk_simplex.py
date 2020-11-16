from scipy.optimize import linprog

'''
    0) all constrains <=

    1) given a feasible starting point X. Substitute
    x into the constraints and take note of the binding constraints

    2) perform all the partial derivatives and sub in X

    3) A is the constraint matrix of the binding constraints
'''
A = [[0, -1]]

'''
    4) b is the RHS of the binding constraints. b is parallel to A
'''
b = [0]

'''
    5) c is the vector caused by (2)
'''
c = [-24, -2]

'''
    6) x bounds. Given in LP
'''
x_bounds = (-1,1)

res = linprog(c, A_ub=A, b_ub=b, bounds=(x_bounds))

print('Optimal values: ', round(res.fun, ndigits=2),
        '\nx values: ', res.x,
        '\nNumber of iterations performed: ', res.nit,
        '\nStatus: ', res.message)