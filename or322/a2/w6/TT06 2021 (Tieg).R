# A bank has 12 tellers that each takes on average 3 minutes to serve a client. Each hour 220 clients arrive on average at the bank and require service.

# Assume the following with regards to the bank:
  
#     - there is ample room in the bank for all waiting client
#     - clients wait in a common queue for service and
#     - all interarrival times and service times are exponential.


# Use the above information to carry out the necessary calculations in R, to enable you to pick the correct statements from the list below. Please note that the selection of any incorrect statement will lead to the deduction of earned marks.

#-----------------------------------------------------------------------------------
# PRE CALCULATIONS
#-----------------------------------------------------------------------------------
# queue type: M/M/s/GD(FIFO)/inf/inf from 20.6 pg 1087
lambda = 220 # c/h
mu = 1/3*60 # (20) c/h
s = 12 # for now

# rho as a function of s
rho = function(s) {
  return(lambda/(mu*s))
}

# Pi0
Pi0 = function(s) {
  D = 0
  for (j in 1:(s-1)) {
    D = D + ( (s*rho(s))^j ) / factorial(j)
  }
  D = D + ( (s*rho(s))^s ) / ( factorial(s)*(1-rho(s)) )
  return (1/D)
}

# Pj
Pj = function(s) {
  return(
    Pi0(s) * ( ( (s*rho(s))^s ) / ( factorial(s)*(1-rho(s)) ) )
  )
}
Pj(7)


#-----------------------------------------------------------------------------------
# QUESTION 1
# 
# The average number of customers who are queuing up and waiting to be served is
#-----------------------------------------------------------------------------------
# We are looking for Lq

# Lq
Lq = function(s) {
  return (
    ( Pj(s)*rho(s) ) / ( 1 - rho(s) )
  )
}

cat(sprintf("Q1. The average number of customers who are queuing up and waiting to be served is %f", Lq(12)))


#-----------------------------------------------------------------------------------
# QUESTION 2
# 
# The average length of time, in terms of minutes, a customer spends inside the bank
#-----------------------------------------------------------------------------------
# We are looking for W. Time inside the bank would be, being served and being waiting

W = function(s) {
  return (
    ( Pj(s) ) / ( s*mu-lambda ) + 1/mu
  )
}

cat(sprintf("Q2. The average length of time, in terms of minutes, a customer spends inside the bank %f", W(12)*60)) # Note the scaling for h->m

#-----------------------------------------------------------------------------------
# QUESTION 3
# 
# The probability that there are more than 15 customers inside the bank at any point in time is
#-----------------------------------------------------------------------------------
# I think we will be using our Pij here. Like (1 - Pij(1:15))

Pij = function(s, j) {
  # NOTE: case where j < s
  if (j < s) {
    return (
      ( (s*rho(s))^j * Pi0(s) ) / ( factorial(j) )
    )
  } else {
    # NOTE: case where j >= s
    return (
      ( (s*rho(s))^j * Pi0(s) ) / ( factorial(s)*(s^(j-s)) )
    )
  }
}
p = 1
for (i in 1:15) {
  p = p - Pij(12, i)
}

cat(sprintf("Q3. The probability that there are more than 15 customers inside the bank at any point in time is %f", p))

#-----------------------------------------------------------------------------------
# QUESTION 4
# 
# The probability of a customer spending more than 90 seconds in the queue is
#-----------------------------------------------------------------------------------
# FIXME: There are 2 Pw formulas. I think one is a special case to idk
# Pw isn't apart of this question but it does concernt me
Pw1 = function(s, t) {
  return (
    exp(-mu*t) * (   1 + Pj(s) * ( ( 1-exp(-mu*t*(s-1-s*rho(s))) ) / ( s-1-s*rho(s) ) )   )
  )
}

# NOTE: Only used when (s-1) = s*rho(s)
Pw2 = function(s, t) {
  return (
    exp(-mu*t) * (   1 + Pj(s)*mu*t   )
  )
}

# Here is the actual question. Pwq or P(Wq > t)
# NOTE: (s-1) = s*rho(s), so I think this also has to hold for this Wq
Pwq = function(s, t) {
  return(
    Pj(s)*exp(-s*mu*(1-rho(s))*t)
  )
}
t = (90/60)/60 # 0.025 h
cat(sprintf("Q4. The probability of a customer spending more than 90 seconds in the queue is %f", Pwq(12, t)))

#-----------------------------------------------------------------------------------
# QUESTION 5
# 
# The probability that all tellers are busy is
#-----------------------------------------------------------------------------------
# Here we are looking for Pj or P(j > s). The probability that the number of customers (j) is greater than the number of servers s

cat(sprintf("Q5. The probability that all tellers are busy is %f", Pj(12)))
