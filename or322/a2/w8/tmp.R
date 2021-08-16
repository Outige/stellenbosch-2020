#--------------------------------------------------------------------------
# WINSTON 20.13 Q2
#--------------------------------------------------------------------------

#----------
# 1) determine the number of station and the number of jobs
#----------
S=2 # 2 steps
N=8 # 8 parts


#----------
# 2) determine the mu values of all the stations
#----------
mubar = c(
  8, # parts/minute
  11 # parts/minute
)


#----------
# 3) setup transition probability matrix (from question)
#----------
P = matrix(c(
  0, 1,
  0.9, 0.1
), nrow=2, byrow=TRUE)
colnames(P) = c("Step 1", "Step 2")
rownames(P) = c("Step 1", "Step 2")
P


#----------
# 4) determine the lambda values of all the stations
#----------
#---
# transform P
#---
A=diag(2)-t(P)
A

#---
# leave out 1st column and row (the main column and row)
#---
LK=A[2:2,2:2]
LK

#---
# 1st column minus the 1st index (the main station's index)
#---
RK=-1*A[2:2,1]
RK

#---
# solve for non-main station lambdas
#---
lambda2=solve(LK)%*%RK
lambda2

#---
# add lambda23 and lambda1 to lambdabar
#---
lambdabar=c(1) # lambda1 = 1.0 as this is relative to lambda1 (the main station's lambda)
lambdabar=append(lambdabar,lambda2)
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

