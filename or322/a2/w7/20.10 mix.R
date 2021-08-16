# This does not look like an open queue. it looks like an exponential queue in series

# DR J's Winston 20.10 Problem 7

#Determine how many jobs are in the system thus: what is L_tau?

rbar=c(1/6, # parts/minute
       0, # parts/minute
       0)  # parts/minute

mubar=c(
  1/3, # parts /minute
  1/2, # parts /minute
  1) #parts/minute

# at (1) and (2) just move product along, we do all checking at 3. At (3) 0.2 send back to (2) and 0.1 send back to (1)
P=matrix(c(
  0, 1, 0,
  0, 0, 1,
  0.1, 0.2, 0
), nrow=3, byrow=TRUE)
P

lambdabar=solve(diag(3)-t(P))%*%rbar  #parts/minute

lambdabar

L=function(lambda,mu)return(lambda/(mu-lambda))

L_bar=c()

for(i in 1:3) {
  L_bar[i]=L(lambdabar[i],mubar[i])
}
L_bar

L_tau=sum(L_bar) #parts

cat("The number of parts in the system is on average ",L_tau,"parts.")

########################################################################################

# DR J example
rbar = c(
  3, # c/h
  2, # c/h
  5 # c/h
)

mubar = c(
  10, # c/h
  15, # c/h
  20  # c/h
)

Pij = matrix(c(
  0, 0.4, 0.4,
  0.2, 0, 0.5,
  0.2, 0.4, 0.2
), nrow=3, byrow=TRUE) # FIXME: Why p don't sum to 1?
rownames(Pij) = c("1", "2", "3")
colnames(Pij) = c("1", "2", "3")
Pij

# lambdabar = Inv(I - transpose(Pij)) matrix multiplied by rbar
lambdabar  = solve(diag(3)-t(Pij))%*%rbar 
lambdabar # lamdabar(i) the rate at which customers arrive at station i

L = function(lambda, mu) {
  return(lambda/(mu-lambda))
}

Lbar=c() # this will contain all the Li values
for (i in 1:3) {
  Lbar[i] = L(lambdabar[i], mubar[i])
}
Lbar

Ltau = sum(Lbar)
Wtau = Ltau/sum(rbar)

########################################################################################

# WINSTON 20.10 Q4

rbar=c(
  120,0,0)  # c/h

mubar=c(
  1/20*60*60, # c/h
  1/15*60*60, # c/h
  1/12*60*60) # c/h

P=matrix(c(
  0, 1, 0,
  0, 0, 1,
  0, 0, 0
  ), nrow = 3, byrow = TRUE)
P

lambdabar=solve(diag(3)-t(P))%*%rbar  #parts/minute

lambdabar

L=function(lambda,mu)return(lambda/(mu-lambda))


L_bar=c()

for(i in 1:3)L_bar[i]=L(lambdabar[i],mubar[i])
L_bar

L_tau=sum(L_bar) #parts
Wtau = Ltau/sum(rbar)
0.413*60

cat("The number of parts in the system is on average ",L_tau,"parts.")

# a. 11/3 or 3.666667 students

#-------------------------------------------------------------------------------
# WINSTON 20.10 Q3
#-------------------------------------------------------------------------------
#----------------
# System 1 (my first attempt)
#----------------
rbar = c(
  40, # c/h
  0)
mubar = c(
  1/30*60*60, # c/h
  1/1*60 # c/h 
)

P = matrix(c(
  0, 1,
  0, 0
), nrow=2, byrow=TRUE)


lambdabar = solve(diag(2)-t(P))%*%rbar
# notice how they are both the same lambda as this is a queue in series
lambdabar

L = function(lambda, mu) { # NOTE: This formula is from 20.4 queues, look top left
  return(lambda/(mu-lambda))
}

Lbar = c()
for (i in 1:2) {
  Lbar[i] = L(lambdabar[i], mubar[i])
}
Lbar

Ltau = sum(Lbar)
Ltau
# Wtau = Ltau/sum(rbar)
Wtau = Ltau/sum(rbar)
Wtau

#----------------
# System 1 (optimized attempt attempt)
#----------------
# most of the shortcuts in this attempt are due to the identity that lamda(i) is constant in a system that is in series
lambda = 40 # we know R(1) so we know lambda of the system
mubar = c(
  1/30*60*60, # c/h
  1/1*60 # c/h 
)

# we dont need a p matrix as we dont need to solve for lambdabar. we will just work with our lambda

L = function(mu) { # NOTE: This formula is from 20.4 queues, look top left
  return(lambda/(mu-lambda))
}

Lbar = c()
for (i in 1:2) {
  Lbar[i] = L(mubar[i])
}
Lbar

Ltau = sum(Lbar)
Ltau


Wtau = Ltau/sum(rbar)
Wtau

#----------------
# System 2 (my first attempt)
#----------------
# M/M/s=2/GD/inf/inf (20.6 queue)
lambda = 40 # c/h
mu = 1/1.5*60 # c/h
rho = lambda/(mu*s)

Pi0 = function(s) {
  D = 0
  for (j in 0:(s-1)) {
    D = D + ( (s*rho)^j ) / ( factorial(j) )
  }
  D = D + ( (s*rho)^s ) / ( factorial(s)*(1-rho) )
  return (1/D)
}

Pj = function(s) {
  return(
    Pi0(s)*( ( s*rho)^s / (factorial(s)*(1-rho)) )
  )
}

W2 = function(s) { # FIXME why does this method not work to calculate W?
  return(
    Pj(s)/(s*mu - lambda) + 1/mu
  )
}
W2(2)

# FIXME: why do we have to use this L method to calculate lambda?
L_q=Pj(2)*rho/(1-rho)
L_s=lambda/mu
L=L_q+L_s
W2 = L/lambda
W2


# CLOSING REMARKS: this question is almost complete. just need to figure out where i go wrong with the 2nd system

#------------------------------------------------------------------------------
#Winston 20.10 Problem 3

#The W of System 1 and System 2 needs to be compared
#to establish the system with the lowest W

#System 1 calculations

#System 1 consists of two M/M/1 systems in series

lambda=40 #c/h

mu_1=3600/30 #c/h

mu_2=3600/60 #c/h

L_1=lambda/(mu_1-lambda)

L_2=lambda/(mu_2-lambda)

L_tau=L_1+L_2

W_System1=L_tau/lambda #hour


#System 2 calculations

#System 2 is a M/M/2 system

lambda=40 #c/h

mu=3600/90 #c/h

s=2

rho=lambda/(s*mu)

i=0:(s-1)

pi_0=1/(sum((s*rho)^i/factorial(i))+(s*rho)^s/(factorial(s)*(1-rho)))

P_jges=(s*rho)^s/(factorial(s)*(1-rho))*pi_0

L_q=P_jges*rho/(1-rho)

L_s=lambda/mu

L=L_q+L_s

W_System2=L/lambda


if(W_System1<W_System2){
  cat("A customer spends the least time in System 1.")}else{
    cat("A customer spends the least time in System 2.")}













