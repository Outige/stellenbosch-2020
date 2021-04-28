#0-------------------------------------
#0 Exhaustive states
#0-------------------------------------
#-------------------------------------
# (1) Construct P matrix
#-------------------------------------
P = matrix(
    c(
        0.1, 0.8, 0, 0, 0.1, 0,
        0, 0.1, .85, 0, 0.05, 0,
        0, 0, 0.15, 0.8, 0.05, 0,
        0, 0, 0, 0.1, 0.05, 0.85,
        0, 0, 0, 0, 1, 0,
        0, 0, 0, 0, 0, 1
    ), nrow=6, byrow=TRUE
)
colnames(P) = c('F.', 'So.', 'J.', 'Sen.', 'Q.', 'G.')
rownames(P) = c('F.', 'So.', 'J.', 'Sen.', 'Q.', 'G.')

#-------------------------------------
# (2) Extract Q and R
#-------------------------------------
Q = P[1:4, 1:4]
R = P[1:4, 5:6]

#-------------------------------------
# (3) Calculate F = Inv(I-Q)
#-------------------------------------
F = solve(diag(4) - Q)

# With f calculated we can calculate F[i,j] = how long
# a person will spend in state j if they started in i

#-------------------------------------
# (4) Calculate A = F.R
#-------------------------------------
A = F%*%R

# A[i][j] is the probability of starting in trainsiant
# state i and ending up in absorbing satete j

#0-------------------------------------
#0 Fututre states
#0-------------------------------------
# Question will be as so: what is the probability
# that given we start in state i at t0 we will
# be in state j at t1

# What you need to do is raise your matrix to the
# power of t1-t0, then on your new matrix P'[i, j]

P3 = P%*%P%*%P

#0-------------------------------------
#0 Steady state distribution
#0-------------------------------------
# We just using a new P because I think there are requirements on our P matrix.
# The 1st one didn't seem to work
P = matrix(c(
    0.1, 0.4, 0.1, 0.2, 0.2,
    0.3, 0.4, 0.1, 0.1, 0.1,
    0.2, 0, 0.1, 0.6, 0.1,
    0.1, 0.1, 0.1, 0.1, 0.6,
    0.2, 0.5, 0.2, 0, 0.1
), nrow=5, byrow=TRUE)
colnames(P) = 1:5
rownames(P) = 1:5

#-------------------------------------
# (1) P-I
#-------------------------------------
Pmod = P-diag(5)

#-------------------------------------
# (2) Replace any column (remembering which one) with all 1s
#-------------------------------------
Pmod[,1] = 1

#-------------------------------------
# (3) Invert our modified matrix
#-------------------------------------
Pmod = solve(Pmod)

#-------------------------------------
# (4) Pre-multiply by a vector of all 0s. With a 1 in the column of 1s
#-------------------------------------
Pij = c(1,0,0,0,0)%*%Pmod

#-------------------------------------
# Questions we can answer with Pij
#-------------------------------------
# (1) Pij[i] is the percentage time we will spend in state i
# (2) Pij is also used in mean first passage times

#0-------------------------------------
#0 Mean first passage times
#0-------------------------------------
#-------------------------------------
# (TYPE 1) Mii
#-------------------------------------
# Mii: minimum expected number of transitions before we get back
# to our original state
# Mii = 1/Pij[i]
M11 = 1/Pij[1]

# Mij: going from state i, what is the expected number of
# transitions before we get to state j
#2 FIXME: Never learned this

#0-------------------------------------
#0 M/M/1/GD/INF/INF queues
#0-------------------------------------
#-------------------------------------
# (1) Calculate lambda
#-------------------------------------
# lambda is arrival rate

# Calculate your lambda in customers/time
# The time unit you use doesn't matter, just
# make sure your mu and questions all agree on the same unit

# if you get lambda in time/customers then you actually got
# given lambda^-1 and you can inverse this to get c/t

# example every 5 minutes 1 customer arrives. Turn to customers/hour
# lambda = 5/1 min/c = 1/5 c/min = 60/5 c/h = 30 c/h

#-------------------------------------
# (2) Calculate mu
#-------------------------------------
# mu is service rate

# everything in (1) applies in 2

#-------------------------------------
# (3) Calculate rho
#-------------------------------------
# rho = lambda/mu

# this is the traffic intensity constant

# it is used in quite a few of this sections equations

#-------------------------------------
# (4) Plug and play formulas
#-------------------------------------
# now you have lambda, mu and rho just use the formulas

#0-------------------------------------
#0 M/M/1/GD/C/INF queues
#0-------------------------------------
# The main diference with these queues is that we constrain
# the number of people in the system

# our formulas change but they're pretty easy to use still

# the biggest thing tho. Is that our lambda is technically
# an over estimation. Our lambda is only true if no customer
# gets refused service because the queue is too busy.

# so we have to deal with "effective lambda". This is on
# the formula sheet techincally

# this "tricky" question the book likes to ask is what is
# the average number of people served by the system? From
# our formulas we would think Ls, but it's actually lambda_effective

#0-------------------------------------
#0 Poisson
#0-------------------------------------
#-------------------------------------
# (1) Calculate lambda like in queues
#-------------------------------------

#-------------------------------------
# (2) Calculations by hand
#-------------------------------------
# You can use the formula on the page to
# get then probability that N = n
# if we want N <= n then you have to sum upto n

#-------------------------------------
# (3) Sum P(N <= n) by ppois
#-------------------------------------
lambda=2
t=3
n=5
ans = ppois(n, lambda*t, lower.tail=TRUE)

#-------------------------------------
# (3) Sum P(N = n) by ppois
#-------------------------------------
ans = ppois(n, lambda*t, lower.tail=TRUE)-ppois(n-1, lambda*t, lower.tail=TRUE)

#0-------------------------------------
#0 Exponential
#0-------------------------------------
# This section has been done by hand mostly. You use the formula from the page,
# and calculate P(x1 <= X <= x2). The probability that the next arrival will
# be between x1 and x2

#0-------------------------------------
#0 Birth death & indiana bell
#0-------------------------------------
#-------------------------------------
# (1) Draw out your diagram
#-------------------------------------
# Your states
# Then your lambdai start from 0 to next state and end a (n). after (n) -> 0
# Your mui start at n and make their way down to mu1 at (1). Before (0) -> 0

#-------------------------------------
# (2) calculate MUj and LAMBDAj (starting with mu1 and lambda0)
#-------------------------------------
# MUj = mu1*j
# lambdaj might just be lambda1

#-------------------------------------
# (3) calculate Cj
#-------------------------------------
# Use the formula to create a cj matrix

#-------------------------------------
# (4) calculate Pi0
#-------------------------------------
# Use the formula to get Pi0
# hint: sum(Cj)

#-------------------------------------
# (5) calculate Pj?
#-------------------------------------

#-------------------------------------
# INDIANA BELL EXAMPLE
#-------------------------------------
lambda0 = 1700 #c/h
mu1 = 30 #c/h

LAMBDAj = c()
for (j in 1:100) LAMBDAj[j] = lambda0
LAMBDAj = rep(lambda0, 100) # nicer way to write it

MUj = c()
for (j in 1:75) MUj[j] = mu1*j
for (j in 76:100) MUj[j] = mu1*75
MUj = c(c(1:75)*mu1, rep(mu1*75, 25)) # nicer way to write it

Cj = c()
Cj[1] = lambda0/mu1
for (j in 2:100) Cj[j] = Cj[j-1]*(LAMBDAj[j]/MUj[j])

Pi0 = 1/(1+sum(Cj))
Pij = c()
for (j in 1:100) Pij[j] = Cj[j]*Pi0

# Q1
# sum(Pij[75:100])

# Q2
# Pij[100]

# Q3 average number of call in the systen at any moment
# c(1:100) %*% Pij

# Q4a) The average number of operators that are busy at any moment is
# c(1:75) %*% c(Pij[1:75]) + sum(Pij[76:100])*75

# Q4b) The average number of operators that are busy at any moment is
# 1:25 %*% Pij[76:100]

#0-------------------------------------
#0 Work force planning
#0-------------------------------------
#-------------------------------------
# (1) Extract Q from Q
#-------------------------------------
P = matrix(c(
    2,7,0,3,0,
    1,1,7,2,1,
    1,0,1,1,9,
    0,0,0,1,0,
    0,0,0,0,1
), nrow=5, byrow=TRUE)
colnames(P)=c('1', '2', '3', 'V', 'G')
rownames(P)=c('1', '2', '3', 'V', 'G')
Q = P[1:3, 1:3]
Q

# hbar is the amount that you need to add to each state to get your required bbar
# bbar is the amount that will be present at each satate

#-------------------------------------
# (2) Calculate hbar from bbar
#-------------------------------------
bbar = c(500, 450, 400)
hbar = bbar %*% (diag(3)-Q)
rownames(hbar) = c('hbar')
hbar

#-------------------------------------
# (3) Calculate bbar from hbar
#-------------------------------------
bbar = hbar %*% solve(diag(3)-Q)
rownames(bbar) = c('bbar')
bbar


#0-------------------------------------
#2 No-memory property
#0-------------------------------------

#0-------------------------------------
#0 Transient state, ergodic
#0-------------------------------------

# i and j COMUNICATE:
# i is reachable from j and j is reachable from i

# CLOSED SET:
# once in the set you can't leave

# ABSORBING SATE:
# self loop of 1

# TRANSIENT SATE:
# there is a way to leave i and never return

# RECURENT STATE:
# NOT transient

# PERDIODIC?

# ERGODIC:
# If all states are recurrent, aperiodic, and communicate

#0-------------------------------------
#2 Week 1
#0-------------------------------------


#0-------------------------------------
#2 Theory from past paper
#0-------------------------------------