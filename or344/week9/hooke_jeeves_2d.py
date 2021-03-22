# -*- coding: utf-8 -*-
"""
Created on Sun Sep 27 10:39:00 2020

@author: 20304269
"""

import numpy as np

#################################
#   Remember to change          #
#   func and stop condition     #
#################################

def func(x1, x2):
    # f = 3 * x1**2 - 2 * x1 * x2 + x2**2 + 4 * x1 + 3*x2
    # f = (2.71-x)**2 + (1-y)**2
    f = (3-4*x)/(1+x**2)
    return f

stop_condition = 0.4

#initial b
# starting point
b1 = np.array([0,0])
fb1 = func(b1[0],b1[1])

#step length
h1 = 1
h2 = 1 

#unit verctors (2D)
e1 = np.array([1,0])
e2 = np.array([0,1])

#placeholder values
temp = b1
ftemp = fb1
best = b1
fbest = fb1
bkeep = b1 #used for pattern moves
fkeep = fb1 
pmove = False

##Scouting move
print("Scout: b1 = ", b1, ", f(b1) = ", fb1)

i = 2

#Stop condition
while h1 >= stop_condition:

    ##Evaluation in the first dimension
    direc = temp + h1 * e1 ### Scout V(i)
    if func(direc[0],direc[1]) < ftemp: ### success
        temp = direc
        ftemp = func(direc[0],direc[1])
        print("f", direc, "=", func(direc[0],direc[1]), " (S)")
    else: ### failure
        print("f", direc, "=", func(direc[0],direc[1]), " (F)")    
        direc = temp - h1 * e1
        
        if func(direc[0],direc[1]) < ftemp: #success
            temp = direc
            ftemp = func(direc[0],direc[1])
            print("f", direc, "=", func(direc[0],direc[1]), " (S)")
        else: #failure
            print("f", direc, "=", func(direc[0],direc[1]), " (F)")    
    
    ##Evaluation in the second dimension
    direc = temp + h2 * e2 
    if func(direc[0],direc[1]) < ftemp: #success
        temp = direc
        ftemp = func(direc[0],direc[1])
        print("f", direc, "=", func(direc[0],direc[1]), " (S)")
    else: #failure
        print("f", direc, "=", func(direc[0],direc[1]), " (F)")    
        direc = temp - h2 * e2
        
        if func(direc[0],direc[1]) < ftemp: #success
            temp = direc
            ftemp = func(direc[0],direc[1])
            print("f", direc, "=", func(direc[0],direc[1]), " (S)")
        else: #failure
            print("f", direc, "=", func(direc[0],direc[1]), " (F)")    
            
    print("New basis b", i, " = " , temp, " with f(b", i, ") = ", ftemp )
        
    ###Pattern move
    if not np.array_equal(temp, bkeep):
        if ftemp >= fkeep:
            temp = bkeep
            print("Pattern move failed, back to b", i-1, "let b", i, " = b", i-1)
            print("Scouting move around b", i , " = ", bkeep)
            pmove = False
        else:
            print("b", i, " is not equal to b", i-1, "therefore do a pattern move:")
            pmove = True
            p = 2 * temp - bkeep
            fp = func(p[0],p[1])    
            best = temp
            fbest = ftemp   
            print("Pattern move around p = ", p, "with f(p) = ", fp)
            temp = p
            ftemp = fp 
            bkeep = best #update bkeep
            fkeep = fbest   
    
    else: #what if temp is equal to bkeep? #have we done a move?
        if pmove == True:
            temp = bkeep
            print("Pattern move failed, back to b", i-1, " let b", i, "= b", i-1)
            print("Scouting move around b", i, " = ", bkeep)
            pmove = False
        else:
            print("Scouting move around b", i-1, "failed, therefore halve the step length")
            h1 = h1/2
            h2 = h2/2
            if h1 < stop_condition: #same value
                print("Stop, x* =", best, " with f(x*) = ", fbest)                
            else:
                print("Scouting move around b", i-1, " = ", bkeep, " with f(b", i-1, ") = ", fkeep)
          
    i += 1