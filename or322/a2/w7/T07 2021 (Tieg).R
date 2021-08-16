# FIXME: I still need to fill in all the questions etc

#-----------------------------------------------------------------------------------
# WINSTON 20.8 PROBLEM 1
#-----------------------------------------------------------------------------------
# queue: M/G/1/GD/inf/inf
# most of the formulas of 20.8 are the same as 20.7

# given information
lambda = 20 # c/h
mu = 1/2 * 60 # (30) c/h

rho = lambda/mu # not really on formula sheet
var = 1/mu^2# (var=sigma^2) 1/mu^2 # E(s) = 1/mu; var = E(s)^2 = (1^2)/(mu^2)
# sigma is rate t which served. 20.7 lambda/mu, 20.8 1/mu 1 as only 1 server
# FIXME: We are still unsure if var = 1/mu^2 (as ar = E^2)

# we need to calculate Lq
Lq = ( lambda^2 * var + rho^2 ) / ( 2*(1-rho) )

cat(sprintf("20.8 Q1: %f", Lq))

# FIXME: This is incorrect acording to the memo. 2/3 Cars is the correct answer. Maybe the answer in the book is just wrong?


#-----------------------------------------------------------------------------------
# WINSTON 20.8 PROBLEM 3
#-----------------------------------------------------------------------------------
#----------
# a.
#----------
# queue: M/G/1/GD/inf/inf

lambda = 40 # c/h
Es = ( 0.95*1 + 0.05*2.5 ) # E(s) min/car
mu = 1/Es * 60 # E(s) = 1/mu min/car; mu = 1/E(s) car/min, *60 to get c/h;
rho = lambda/mu

# var(s) = 1/mu^2
var = 1/mu^2

Lq = ( lambda^2*var + rho^2 ) / ( 2*(1-rho) )
Wq = Lq/lambda
cat(sprintf("20.8 Q3a: %f", Wq))

#----------
# b.
#----------
Es = ( 1*1 ) # E(s)
mu = 1/Es * 60 # E(s) = 1/mu min/car; mu = 1/E(s) car/min, *60 to get c/h;
rho = lambda/mu

# var(s) = 1/mu^2
var = 1/mu^2

Lq = ( lambda^2*var + rho^2 ) / ( 2*(1-rho) )
Wq = Lq/lambda
cat(sprintf("20.8 Q3a: %f", Wq))


#-----------------------------------------------------------------------------------
# WINSTON 20.10 PROBLEM 1
#-----------------------------------------------------------------------------------
#-----
# Option 1
#-----
# queue(FIXME: I couldn't really figure this out myself): M/M/s/GD/inf/inf (apparently) which is a 20.6 queue, but I thought we were doing 20.10?
lambda = 4.8 # c/h
mu = 60/15 # (4) c/h
s = 3 # given
rho = lambda/(mu*s)

 # we are looking for W, but we have to do a bunch of pre-calculations

# Pi0. I made this a function of s out of habit
Pi0 = function(s) {
  D = 0
  for (j in 0:(s-1)) {
    D = D + ( (s*rho)^j ) / (factorial(j)) 
  }
  D = D + ( (s*rho)^s ) / ( factorial(s)*(1-rho) )
  return (1/D)
}

# P(j >= s)
Pj = function(s) {
  return (
    Pi0(s)*( ((s*rho)^s) / ( factorial(s)*(1-rho) ) )
  )
}

# Finally W
W = ( Pj(s)/(s*mu-lambda) ) + 1/mu
cat(sprintf("20.10 Q1 option 1: %f", W))
W*60

#-----
# Option 2
#-----
# open queue you can start at any station
  # potentially different lambdas, lambda1, lambda2(internal lambdas) and 1 lambda which is for entire system
# series you must do in specific order
  # same lambda for all

# starts as self service (20.7) then another normal 20.6 queue, we will jusst sum the W


#--
# QUEUE 2.1 - SELF SERVICE (20.7)
#--
# 20.7 and 20.8 use similar equations
# 20.7 is uneque in that var approxes to lambda/mu

# FIXME: This all seems wrong. I can't find good equations for this section. We did find equations for L and W. I think 0 for Lq and Wq, then W = Ws and L = Ls
lambda = 4.8 # c/h
mu = 1/65 * 60 # c/h
#rho = lambda/mu # i dont know if this is the correct formula
#var = lambda/mu # E ~ V ~ lambda/mu, also sigma^2 = var
#Lq = ( lambda^2*var + rho^2 ) / ( 2*(1-rho) )
#Wq = Lq/lambda
#W1 = Wq + 1/mu
#W1 # FIXME: this seems wrong very
W1=1/mu # NOTE this comes from text book. couldn't be found any where in formula sheet or in notes
W1

#--
# QUEUE 2.2 (20.6)
#--
lambda = 4.8 # the same as in series
mu = 1/4*60 # c/h
s = 3 # given

rho = lambda/(mu*s)

Pi0(s)
Pj(s)

W2 = ( Pj(s)/(s*mu-lambda) ) + 1/mu # using same equations from queue option 1
W2

# PRINTS
cat(sprintf("option 2 = %f + %f = %f(Op2) vs %f(Op1)", W1, W2, W1+W2, W))
cat(sprintf("option 1 is faster than option 2"))


#-----------------------------------------------------------------------------------
# WINSTON 20.10 PROBLEM 6
#-----------------------------------------------------------------------------------

# 3 stations in series
# open has multiple lambdas, closed has constant
# open has multiple Rs

#-----
# PRE CALCULATIONS
#-----
rbar = c(
  10, # jobs/hour
  0, # jobs/hour
  0 # jobs/hour
)

mubar = c(
  20, # jobs/hour
  20, # jobs/hour
  20 # jobs/hour
)

P = matrix(c(
  0,   1,   0,
  0.1, 0,   0.9,
  0,   0.2, 0
), nrow=3, byrow=TRUE)
P

lambdabar = solve(diag(3)-t(P))%*%rbar
lambdabar

#-----
# a)
#-----
# so each each queue is an M/M/1/GD/inf/inf queue (20.4). So any queue specific things like Pi0 etc which is what we have to do here
Pi0bar = c()
for (i in 1:3) {
  rho = lambdabar[i]/mubar[i]
  Pi0bar[i] = 1-rho
}
Pi0bar


#-----
# b)
#-----
L = function(lambda, mu) {
  return(lambda/(mu-lambda)) # this formula is out up top
}

Lbar = c()
for (i in 1:3) {
  Lbar[i] = L(lambdabar[i], mubar[i])
}
Lbar

Ltau = sum(Lbar)
Ltau

cat(sprintf("20.10 Q6a: %f jobs", Ltau))

#-----
# c)
#-----
Wtau = Ltau/sum(rbar)

Wtau1 = 0

for (i in 1:3) {
  Wtau1 = Wtau1 + Lbar[i]/lambdabar[i]
}

cat(sprintf("20.10 Q6a: %f hours or %f minutes", Wtau, Wtau*60))
# FIXME: Wtau1 Wtau2, which is best? I think W1 as its dr jacobs







