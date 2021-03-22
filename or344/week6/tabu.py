import numpy as np
import math

def f(x, function):
    return eval(function)

def feasible(x, c):
    # print(x, c)
    return eval(c)

def is_tabu(i, tabu):
    for x in tabu:
        if x == i:
            return True
    return False

def best_move(function, x, c, tabu):
    moves = []
    best_move = [None, -2147483648, 0]
    for i in range(len(x)):
        moves.append(x.copy())
        moves[i][i] = int(not moves[i][i])
        if feasible(moves[i], c) and not is_tabu(i, tabu) and f(moves[i], function) > best_move[1]:
            best_move = [moves[i], f(moves[i], function), i]
    return best_move

def best_move_min(function, x, c, tabu):
    moves = []
    best_move = [None, 2147483647, 0]
    for i in range(len(x)):
        moves.append(x.copy())
        moves[i][i] = int(not moves[i][i])
        if feasible(moves[i], c) and not is_tabu(i, tabu) and f(moves[i], function) < best_move[1]:
            best_move = [moves[i], f(moves[i], function), i]
    return best_move

def shift_tabu(tabu, new):
    tabu[1:] = tabu[:-1]
    tabu[0] = new
    return tabu

def tabu_calc(function, x, c, tabu, lb, tmax, itr, bmax):
    z_old = f(x, function)
    x_old = x.copy()
    lb_old = lb

    if bmax:
        best = best_move(function, x, c, tabu)
    else:
        best = best_move_min(function, x, c, tabu)
    
    if best[0] == None:
        return
    
    x = best[0]
    if bmax:
        if best[1] > lb:
            lb = best[1]
    else:
        if best[1] < lb:
            lb = best[1]

    
    if itr == 0:
        print( "%-6s %-20.4s %-10.4s %-10.4s %-10.4s %-10.4s"%("t", "x(t)", "z(t)", "LB", "compl", "delta"))


    if itr == tmax:
        print( "%-6d %-20s %-10d %-10d %-10s %-10s"%(itr, str(x_old), z_old, lb_old, "STOP", ""))
        return
    print( "%-6d %-20s %-10d %-10d %-10d %-10d"%(itr, str(x_old), z_old, lb_old, best[2]+1, best[1]-z_old))

    tabu = shift_tabu(tabu, best[2])
    tabu_calc(function, x, c, tabu, lb, tmax, itr+1, bmax)
    

if __name__ == '__main__':
    imax = 2
    tabu = [-1] * imax
    function = "7*x[0] + 5*x[1] + 8*x[2] + 3*x[3]"
    x = [0,0,1,0]
    c = "(x[0]*x[1] + x[1]*x[2] + x[2]*x[3] <= 1) and (4*x[0] + 3*x[1] + 6*x[2] + 2*x[3] <= 9)"
    # c = "True and True"
    tmax = 6
    itr = 0
    bmax = True
    tabu_calc(function, x, c, tabu, f(x, function), tmax, itr, bmax)