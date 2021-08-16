#A call centre taking fast-food orders for express delivery receives 20 calls per minute. There are currently 7 operators taking orders. The average call duration is 20 seconds. (Time between calls and call durations are exponential distributed.) Incoming calls finding all operators busy are put on hold until an operator becomes available to take the order. Calls wait in a single queue to be served on a FIFO basis. It can be assumed that all incoming calls will remain on hold until they are served.

#Q1.In the current system, how long does a caller wait in seconds on average, before being able to place the order?
#Q2.From a caller's perspective in the current system, what is the probability of spending more than 80 seconds on the phone, waiting- and ordering time combined?
#Q3.What is the least number of operators that should be employed if the combined waiting and ordering time of callers is to be less than 30 seconds on average?
#Q4.If there were 9 operators, what would the traffic intensity for the call centre as a queueing system be?
#Q5.In the current system, on average how many operators are busy  at any instant?

#---------------------------
# pre working out
#---------------------------

# type of queue: M/M/s/GD/inf/inf

lambda = 20 # call/minute
mu = 3 #call/minute | 1 call/20s -> 3 call/minute
s = 7

# traffic intensity. Made a function of s as s will change
# standard formula: rho = lambda/(s*mu)
# i dont think this is on the formula sheet
rho=function(s)return(lambda/(s*mu))

# Pi zero. Again in terms of s as a function
# standard formula: is quite involved
# located top left of the formula sheet
pi_0 = function(s) {
  D = 0 # denominator
  i = 0:(s-1)
  for (i in i) {
      D = D + ( (s*rho(s))^i )/factorial(i)
  }
  D = D + ((s*rho(s))^s) / (factorial(s)*(1 - rho(s)))
  return(1/D)
}

# This is how my tutor does pi_0
# pi_0=function(s){i=0:(s-1)
# return(1/(sum((s*rho(s))^i/factorial(i))+(s*rho(s))^s/(factorial(s)*(1-rho(s)))))}


P_jges=function(s)return((s*rho(s))^s/(factorial(s)*(1-rho(s)))*pi_0(s))

L_q=function(s)return(P_jges(s)*rho(s)/(1-rho(s)))

#ct=function(s)return(s*10+L_q(s)*50)

#---Q1
# In the current system, how long does a caller wait in seconds on average, before being able to place the order?
#---Q1

# So we are looking for the average wait time in the queue, which we know is Wq

# P(j >= s), which required to calculate Lq. Top right on formula sheet. Note this uses s not j as input
P_jges=function(s) {
  return (
    pi_0(s) * ( ( (s*rho(s))^s ) / ( factorial(s)*(1-rho(s)) ) )
  )
}

# Lq, which is required to calculate Wq. Top right of formula sheet
L_q = function(s) {
  return (
    ( P_jges(s)*rho(s) ) / ( 1-rho(s) )
  )
}

# Wq. Finally the answer. Found top right on formula sheet
Wq = (L_q(7)/lambda)*60 # *60 for minutes to seconds

sprintf("In the current system, a caller will wait %fs on average, before being able to place the order", Wq)

#######################

rho(7)

W=function(s)return(P_jges(s)/(s*mu-lambda)+(1/mu))
W(7)

#Q4
rho(9)

#Q5
L_s=lambda/mu
L_s

#Q2
P_wgt=exp(-mu*4/3)*(1+P_jges(7)*((1-exp(-mu*4/3*(6-(7*rho(7)))))/(6-(7*rho(7)))))
P_wgt


#Q3
s=5:30
mydata=sapply(s,W)
myDF=data.frame(s,mydata)
myDF
#myct=sapply(s,ct)
#myDF=data.frame(s,myct)
#myDF
#cat("The minimum total cost of $",min(myct),"per hour will be attained by appointing",s[match(min(myct),myct)],"tellers.")




#-----------------------------------------------------------------------------
# A2 PREP
#-----------------------------------------------------------------------------

#---------------------------------
# 20.6 PROBLEM 2
#---------------------------------
lambda = 50 # c/d
mu = 60 # c/d
cpt = 100 # cost per teller
cpd = 100 # cost per delay

rho = function(lambda, mu, s) {
  return(
    lambda/(mu*s)
  )
}

rho(lambda, mu, 2)

# we want to min cost. what causes cost?
      # cost = s*cpt + Lq*cpd (note it might be L not Lq)

Pi0 = function(lambda, mu, s) {
  D = 0
  for (j in 0:(s-1)) {
    D = D + (s*rho(lambda, mu, s))^j / factorial(j)
  }
  D = D + (s*rho(lambda, mu, s))^s / ( factorial(s)*(1-rho(lambda, mu, s)) )
  return(1/D)
}
Pi0(lambda, mu, 2)

Pij = function(lambda, mu, s, j) {
  if (j < s) {
    return(
      ( (s*rho(lambda, mu, s))^j*Pi0(lambda, mu, s) ) / (factorial(j))
    )
  } else {
    return(
      ( (s*rho(lambda, mu, s))^j*Pi0(lambda, mu, s) ) / (factorial(s)*s^(j-s))
    )
  }
}
Pij(lambda, mu, 2, 3)

Pj = function(lambda, mu, s) {
  return(
    Pi0(lambda, mu, s)*( (s*rho(lambda, mu, s))^s / ( factorial(s)*(1-rho(lambda, mu, s)) ) )
  )
}
Pj(lambda, mu, 2)

Lq = function(lambda, mu, s) {
  return(
    ( Pj(lambda, mu, s)*rho(lambda, mu, s) ) / ( 1-rho(lambda, mu, s) )
  )
}
Lq(lambda, mu, 2)

cost = function(lambda, mu, s, cpt, cpd) {
  return(
    s*cpt + Lq(lambda, mu, s)*cpd
  )
}

small = 999999999
j = -1

for (s in 1:1000) {
  c = cost(lambda, mu, s, cpt, cpd)
  if (c < small) {
    small = c
    j = s
  }
}

cat(sprintf("%d %f", j, small))

#---------------------------------
# 20.6 PROBLEM 4
#---------------------------------
lambda = 100 # c/h
mu = 50 # c/h
cpt = 5 # $/h
cpd = 20 # $/h

small = 999999999
j = -1
cost(lambda, mu, 1, cpt, cpd)

for (s in 2:1000) {
  c = cost(lambda, mu, s, cpt, cpd)
  if (is.finite(c) && c < small) {
    small = c
    j = s
  }
}
cat(sprintf("%d %f", j, small))


#---------------------------------
# 20.6 PROBLEM 10 (this question is a fucking mess)
#---------------------------------
# Queue: M/M/3/GD/inf/inf
lambda = 50 # c/s
mu = 1/0.03 # c/s

#---
# a)
#---
# ?


#---
# b)
#---
cat(sprintf("b) %f", Pi0(lambda, mu, 3))) # Pi zero - steady state probability that no customers in queue


#---
# c)
#---
# ?
cat(sprintf("a) %f", Pj(lambda, mu, 3))) # P(j > 3) # ?


#---
# d)
#---
cat(sprintf("d) %f", Lq(lambda, mu, 3))) #Lq



#---------------------------------
# 20.6 PROBLEM 11 (this question is a fucking mess as well, fuck)
#---------------------------------
lambda = 200 # c/h
mu = 1/2*60 # c/h

#---
# a)
#---
# we want to find s, such that W < 30
W = function(lambda, mu, s) {
  return(
    Pj(lambda, mu, s)/(s*mu-lambda) + 1/mu
  )
}
W(lambda, mu, 80)


#---
# b)
#---


#---------------------------------
# 20.7 PROBLEM 2
#---------------------------------
lambda = 25 # c/y
mu = 1/4 # c/y
L = lambda/mu
cat(sprintf("20.7 P2) %d", L))


#---------------------------------
# 20.7 PROBLEM 3
#---------------------------------
lambda = 20 # c/y
mu = 1/10 # c/y
L = lambda/mu + 40
cat(sprintf("number of expected firms: %d", L))
L

Pij = function(lambda, mu, j) {
  N = ((lambda/mu)^j)*exp(-lambda/mu)
  D = factorial(j)
  return(N/D)
}

sigma = 0
for (j in 1:300) {
  c = Pij(lambda, mu, j)
  if (is.finite(c)) {
    x[j] = c
    sigma = sigma + c
  }
}

cat(sprintf("P(j > 300) = %.7f ~= 1.0", 1-sigma))


#---------------------------------
# 20.9 PROBLEM 2
#---------------------------------
# Q: M/M/3/GD/3/3
lambda = 1/15 # c/m
mu = 1/10 # c/m
rho = lambda/mu
K = 3

Pi0 = function(lambda, mu, K) {
  sigma = 0
  for (j in 0:K) {
    sigma = sigma + choose(K, j)*(lambda/mu)^j
  }
  return(1/sigma)
}

Pij = function(lambda, mu, K, j) {
  return(
    choose(K, j)*(lambda/mu)^j*Pi0(lambda, mu, K)
  )
}

ans = 1-(Pij(lambda, mu, K, 2)+Pij(lambda, mu, K, 3))
cat(sprintf("a) P(j > 2) = %f", ans))


sigma = 0
for (j in 0:K) {
  sigma = sigma + j*Pij(lambda, mu, K, j)
}
sigma
cat(sprintf("b) L = %f", sigma))


#---------------------------------
# 20.9 PROBLEM 3
#---------------------------------
lambda = 1/100 # c/d
mu = 1/7 # c/d supposed mu
rho = lambda/mu
K = 10000 #?

Pi0 = function(lamnda, mu, K) {
  D = 0
  for (K in 0:K) {
    D = D + choose(K, j)*(lambda/mu)^j
  }
  return(1/D)
}
Pi0(lambda, mu, K)

Pij = function(lambda, mu, K, j) {
  choose(K, j)*(lambda/mu)^j*Pi0(lambda, mu, K, j)
}
Pij(lambda, mu, K, 1)

L = 0
for (j in 0:K) {
  L = L + j*Pij(lambda, mu, K, j)
}

