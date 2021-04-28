#0----------------------------------------------------
#0 PROBLEM 4
#0----------------------------------------------------
lambda = 40 # c/h
mu = 60 # c/h
rho = lambda/mu

#1---
#1 4a
#1---
# We want to calculate Lq, the average number of people waiting
Lq = (lambda^2)/(mu*(mu-lambda))
Lq

#1---
#1 4b
#1---
# We want to calculate W. The average time a person waits in the whole system
L = lambda/(mu-lambda)
W = (1/lambda)*L
W

#1---
#1 4c
#1---
# We want to calculate 1 - Pi0 - Pi1 - Pi2 - Pi3

# get [Pi0, Pi1, Pi2, Pi3]
Pij = c()
for (j in 0:3) {
  Pij[j+1] = (rho^j)*(1-rho)
}

1-sum(Pij)

#0----------------------------------------------------
#0 PROBLEM 6
#0----------------------------------------------------
# This question is actually more dificult than 1st anticipated
# You have to make the leap that L = 5

lambda = 300/73 # babies/day (b/d) = 1500 b/year
L = 5
W = (1/lambda)*L
W