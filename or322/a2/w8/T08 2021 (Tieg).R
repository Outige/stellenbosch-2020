#---------------------------------
# 20.11 Q6
#---------------------------------
#
lambda = 500 # c/h
mu = 1/3*60 # c/h
rho = lambda/mu


Pis = function(s) {
  N = (rho^s)/factorial(s)
  D = 0
  for (j in 0:s) {
    D = D + (rho^j)/factorial(j)
  }
  return(N/D)
}

L = function(s) {
  return (
    (lambda*(1-Pis(s)))/mu
  )
}

profit = function(s) {
  # FIXME: We want to use lambda effective here or L
  return ( (lambda*(1-Pis(s)))*100 - 15*s)
  # return(L(s)*100 - 15*s)
}

max = 0.0
smax = -1
for (i in 1:1000) {
  if (profit(i) > max) {
    max = profit(i)
    smax = i
  }
}
max
smax


#--------------------------------------------------------------------------
# WINSTON 20.11 Q1
#--------------------------------------------------------------------------
# M/M/s/GD/s/inf
lambda = 24 # c/h
mu = 1/20*60 # c/h
rho = lambda/mu # FIXME: is this true?

# we are looking for Pis, the probability that there are already s fire trucks out - this formula is easy to find on the sheet
Pis = function(s) {
  N = (rho^s)/factorial(s)
  D = 0
  for (j in 0:s) {
    D = D + (rho^j) / (factorial(j))
  }
  return(N/D)
}

# so here we are trying to find the specific s value that will result in a queue overflow satate 0.01(1%) of the time
i = 1
while (Pis(i) > 0.01) {
  i = i+1
}

cat(sprintf("20.11 Q1: %d trucks results in %f percent down time", i, Pis(i)))


#--------------------------------------------------------------------------
# WINSTON 20.11 Q6
#--------------------------------------------------------------------------
# M/M/s/GD/s/inf
lambda = 500 # c/h
mu = 1/3*60 # c/h
rho = lambda/mu


Pis = function(s) {
  N = (rho^s)/factorial(s)
  D = 0
  for (j in 0:s) {
    D = D + (rho^j)/factorial(j)
  }
  return(N/D)
}
Pis(1)

lambda_effective = function(s){
  return(
    (lambda*(1-Pis(s)))
  )
}
lambda_effective(1)

profit = function(s) {
  return(100*lambda_effective((s)) - s*15)
}
profit(2)

out = c(0, -1)
for (i in 1:1000) {
  x = profit(i)
  if (x > out[1]) {
    out = c(x, i)
  }
  
  
}
cat(sprintf("20.11 Q6: best profit of, %f, occurs with %d servers", out[1], out[2]))


#--------------------------------------------------------------------------
# WINSTON 20.11 Q8
#--------------------------------------------------------------------------
lambda = 10 # orders/month
mu = 1/1 # orders/month
s = 4 # units
rho = lambda/mu

Pis = function(s) {
  N = (rho^s)/factorial(s)
  D = 0
  for (j in 0:s) {
    D = D + (rho^j)/(factorial(j))
  }
  return(N/D)
}

Pis(s)


#--------------------------------------------------------------------------
# WINSTON 20.13 Q1
#--------------------------------------------------------------------------

#----------
# 1) determine the mu values of all the stations
#----------
mubar = c(
  1/0.039, # jobs/second (mu must be in thing/time)
  1/0.18,  # jobs/second
  1/0.26   # jobs/second
)

#----------
# 2) determine the number of station and the number of jobs
#----------
S=3
N=3


#----------
# 3) setup transition probability matrix (from question)
#----------
P = matrix(c(
  1/20, 13/20, 6/20, # each row sum to 1.0
  1, 0, 0, # every job will return to CPU (main station)
  1, 0, 0 # every job will return to CPU (main station)
), nrow=3, byrow=TRUE)
colnames(P) = c("CPU", "D1", "D2")
rownames(P) = c("CPU", "D1", "D2")
P


#----------
# 4) determine the lambda values of all the stations

# NOTE: we will be solving for 'relative lambdas'. Meaning we will solve lambda2 and lambda3 relative to lambda1(the main station's lambda)
# NOTE: For this step I don't really follow all of the sub steps. They are just the way that we do the lambda_j formula of this section in R
#----------

#---
# transform P
#---
A=diag(3)-t(P)
A

#---
# leave out 1st column and row (the main column and row)
#---
LK=A[2:3,2:3]
LK

#---
# 1st column minus the 1st index (the main station's index)
#---
RK=-1*A[2:3,1]
RK

#---
# solve for non-main station lambdas
#---
lambda23=solve(LK)%*%RK
lambda23

#---
# add lambda23 and lambda1 to lambdabar
#---
lambdabar=c(1) # lambda1 = 1.0 as this is relative to lambda1 (the main station's lambda)
lambdabar=append(lambdabar,lambda23)
lambdabar


#----------
# 5) setup transition probability matrix (from question)
#----------
rhobar=lambdabar/mubar # lambdabar[i]/mubar[i]
rhobar


#----------
# 6) create a vector with all the possible states (nij)
#----------
#install.packages("partitions") # you need to uncomment this if you have never installed the package before
library(partitions) # import function from newly imported package
all_states=t(compositions(N,S))
all_states


#----------
# 7) calculate tau of the system

# NOTE: tau is the number of possible states
# NOTE: this could just be figured out by counting all_states
#----------
tau=choose(S+N-1,S-1)
tau


#----------
# 8) calculate G(N)

# NOTE: This is more of an intimediate value used in a later formula
#----------
GN = 0
for (i in 1:tau) {
  product = 1
  for (j in 1:S) {
    product = product*(rhobar[j]^all_states[i,j])
  }
  GN = GN + product
}
GN

#----------
# 9) calculate (capital greek pi)N(ni) or PIbar
#----------
PIbar = c()
for (i in 1:tau) {
  top = 1 # numerator
  for (j in 1:S) {
    top = top*(rhobar[j]^all_states[i, j])
  }
  PIbar[i] = top/GN
}
PIbar
sum(PIbar) #should sum to 1


#----------
# 10) create gamma(looks like an r) of s and eta(fancy n) function
#----------
gamma = function(s, eta) {
  out = 0
  # only sum where nij = eta
  for (i in 1:tau) {
    if (all_states[i, s] == eta) {
      out = out + PIbar[i]
    }
  }
  return(out)
}

gamma(3,3) #P(3 parts @station 3)


#----------
# a)
#----------
# setup blank Pij matrix
Pij=matrix(rep(0,S*(N+1)),ncol=N+1)
rownames(Pij)=c("CPU", "D1", "D2")
colnames(Pij)=c("0","1","2","3")

# calculate Pij values using our gamma function
for(s in 1:S){
  for(eta in 0:N) {
    Pij[s,eta+1]=gamma(s,eta)
  }
}
Pij

# check that each row sums up to 1
check = c()
for (i in 1:S) {
  check[i] = 0
  for (j in 1:(N+1)) {
    check[i] = check[i] + Pij[i, j]
  }
}
check

#----------
# b)

# NOTE: when want to calculate L at each station
#----------
Lbar = Pij[1:S,1] # you could just use c() here, I didn't so that I could keep my labels
for (s in 1:S) {
  Lbar[s] = 0
  for (eta in 0:N) {
    Lbar[s] = Lbar[s] + eta*gamma(s, eta)
  }
}
Lbar


#----------
# c)
#----------
# idle probability array
idlebar = Pij[1:S,1]
busybar = 1-idlebar


#----------
# d)
#----------
omegabar = busybar*mubar
omegabar

