#0-------------------------------------------------
#0 PROBLEM 2
#0-------------------------------------------------
# queue = M/M/1/GD/4/INF
lambda = 40 # c/h
mu = 15 # c/h
rho = lambda/mu
c = 4

#1---
#1 2a
#1---
# We are looking for Lq
# L = Lq + Ls; Lq = L-Ls
L = ( rho * ( 1 - (rho^c)*(1+c) + c*rho^(c+1) ) ) / ( (1-rho^(c+1))*(1-rho) )
cat(sprintf("L: %.4f\n", L))

Pi0 = (1-rho)/(1-rho^(c+1))
Ls = 1-Pi0
cat(sprintf("Ls: %.4f\n", Ls))


Lq = L - Ls
cat(sprintf("a) Lq: %.4f\n\n", Lq))


#1---
#1 2b
#1---
# This was my 1st attempt at b). It is wrong because
# I answered The average number of people being served huh

# Pi0 = (1-rho)/(1-rho^(c+1))
# Ls = 1-Pi0
# cat(sprintf("b) Ls: %.4f\n\n", Ls))

# What we really want to calculate is effective lambda
Pic = (rho^c)*Pi0
lmabda_effective = lambda*(1- Pic)
cat(sprintf("b) Effective Lambda: %.4f\n\n", lmabda_effective))


#1---
#1 2c
#1---
Pic = (rho^c)*Pi0
W = (1/(lambda*(1-Pic))) * L
cat(sprintf("c) W: %.4f\n\n", W))
