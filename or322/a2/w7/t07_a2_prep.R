#----------------------------------------------------------------------------------------------------
# SECTION 20.8
#----------------------------------------------------------------------------------------------------
# DETAILS:
#   start page: ?
#   question page: ?
#   tut questions: 1, 3
#   solutions: ?
#   Q: M/G/1/GD/∞/∞

# SUMMARY:
#   - single server queues
#   - inter arival times exponential
#   - not birth death

#--------------------------------------------------
# PROBLEM 1
#--------------------------------------------------
# An average of 20 cars per hour arrive at the drive-in
# window of a fast-food restaurant
#     a) If each car’s service time
#     is 2 minutes, how many cars (on the average) will be waiting
#     in line? Assume exponential interarrival times.

lambda = 20 # c/h, NOTE: exponential = M
mu = 1/2*60 # c/h, NOTE: unspecified = G
s = 1 # 1 drive in window
# Q: M/G/1/GD/∞/∞

Lq = function(lambdam, mu) {
    sigmasq = (1/mu)^2
    lambdasq = lambda^2
    rho = lambda/mu
    rhosq = rho^2
    return(
        ( lambdasq*sigmasq+rhosq ) / ( 2*(1-rho) )
    )
}
cat(sprintf("20.8: Problem 1: On average there will be\u001b[36m %f \u001b[0mcars waiting in line\n\n", Lq(lambda, mu)))


#--------------------------------------------------
# PROBLEM 3
#--------------------------------------------------
# An average of 40 cars per hour arrive to be painted at a
# single-server GM painting facility. 95% of the cars require
# 1 minute to paint; 5% must be painted twice and require 2.5
# minutes to paint. Assume that interarrival times are
# exponential.
#     a) On the average, how long does a car wait before be-
#     ing painted?
#     b) If cars never had to be repainted, how would your
#     answer to part (a) change?

lambda = 40 # c/h
s = 1 # single server
muav = 1/(0.95*1 + 0.05*2.5)*60 # c/h
# Q: M/G/1/GD/∞/∞

Wq = function(lambda, mu) {
    return(Lq(lambda, mu)/lambda)
}
cat(sprintf("20.8: Problem 3a: On average a car waits\u001b[36m %f \u001b[0mhours or\u001b[36m %f \u001b[0minutes before getting painted\n",
Wq(lambda, muav), Wq(lambda, muav)*60))

muav = 1/(1.0*1)*60 # c/h
cat(sprintf("20.8: Problem 3b: On average a car waits\u001b[36m %f \u001b[0mhours or\u001b[36m %f \u001b[0minutes before getting painted\n\n",
Wq(lambda, muav), Wq(lambda, muav)*60))


#----------------------------------------------------------------------------------------------------
# SECTION 20.10
#----------------------------------------------------------------------------------------------------
# DETAILS:
#   start page: ?
#   question page: ?
#   tut questions: 1, 6
#   solutions: ?
#   Q: ?

# SUMMARY: # TODO
#   - customer needs to served by K stations in any order before completion

#--------------------------------------------------
# PROBLEM 1
#--------------------------------------------------
# A Social Security Administration branch is considering
# the following two options for processing applications for
# social security cards:
# The interarrival time of applicants is exponential, and an
# average of 4.8 applicants arrive each hour
#   a) Which option will get applicants out of the office more quickly?

# OPTION 1:
# Three clerks process applications in parallel from
# a single queue. Each clerk fills out the form for the appli-
# cation in the presence of the applicant. Processing time is
# exponential with a mean of 15 minutes. Interarrival times
# are exponential.

#---------
# option 1
#---------
# standard 20.6 Q

s = 3
mu = 1/15*60 # c/h, exponential(M)
lambda = 4.8 # c/h, exponential(M)
# Q: M/M/3/GD/∞/∞
rho6 = function(lambda, mu, s) {
    return(
        lambda/(mu*s)
    )
}

PI06 = function(lambda, mu, s) {
    D = 0
    for (j in 0:(s-1)) {
        D = D + (s*rho6(lambda, mu, s))^j/factorial(j)
    }
    D = D + (s*rho6(lambda, mu, s))^s/(factorial(s)*(1-rho6(lambda, mu, s)))
    return(1/D)
}

Pj6 = function(lambda, mu, s) {
    return(
        PI06(lambda, mu, s)*( (s*rho6(lambda, mu, s))^s/(factorial(s)*(1-rho6(lambda, mu, s))) )
    )
}

W6 = function(lambda, mu, s) {
    return(Pj6(lambda, mu, s)/(s*mu - lambda) + 1/mu)
}
cat(sprintf("20.10: Problem 1: Option 1 is\u001b[36m %f \u001b[0mminutes\n", W6(lambda, mu, s)*60))


# OPTION 2:
# Each applicant first fills out an application with-
# out the clerk’s help. The time to accomplish this is expo-
# nentially distributed, with a mean of 65 minutes. When the
# applicant has filled out the form, he or she joins a single line
# to wait for one of the three clerks to check the form. It takes
# a clerk an average of 4 minutes (exponentially distributed)
# to review an application.

#---------
# option 2
#---------
# looks like a self service 20.7 Q into a parallel 20.6 Q, so sum W7 + W6
s = 3
lambda = 4.8 # c/h
mu1 = 1/65*60 # c/h
mu2 = 1/4*60 # c/h

W7 = function(lambda, mu) {
    return(1/mu)
}
cat(sprintf("20.10: Problem 1: Option 2 is\u001b[36m %f \u001b[0mminutes\n\n", W6(lambda, mu2, s)*60 + W7(lambda, mu1)*60))


#--------------------------------------------------
# PROBLEM 6
#--------------------------------------------------
# Consider a queuing system consisting of three stations
# in series. Each station consists of a single server, who can
# process an average of 20 jobs per hour (processing times at
# each station are exponential). An average of 10 jobs per
# hour arrive (interarrival times are exponential) at station 1.
# When a job completes service at station 2, there is a .1
# chance that it will return to station 1 and a .9 chance that it
# will move on to station 3. When a job completes service at
# station 3, there is a .2 chance that it will return to station 2
# and a .8 chance that it will leave the system. All jobs
# completing service at station 1 immediately move on to
# station 2.
#     (a) Determine the fraction of time each server is busy.
#     (b) Determine the expected number of jobs in the
#     system.
#     (c) Determine the average time a job spends in the
#     system.

# NOTE: 3 stations in series. Classic 20.10. lambda is constant
# lambda = 10 # c/h, M - lambda1 which sets the whole system
# mu = 20 # c/h, M - all mu
# x = matrix(c(
#     0, 1, 0,
#     0.1, 0, 0.9,
#     0, 0.2, 0
# ), nrow=3, byrow=TRUE)
# x
# # Q: ?

# 3 stations in series
# open has multiple lambdas, closed has constant
# open has multiple Rs, open is like a general version?

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

lambdabar = solve(diag(3)-t(P))%*%rbar

#-----
# a)
#-----
# so each each queue is an M/M/1/GD/inf/inf queue (20.4). So any queue specific things like Pi0 etc which is what we have to do here
Pi0bar = c()
for (i in 1:3) {
  rho = lambdabar[i]/mubar[i]
  Pi0bar[i] = 1-rho
}
cat(sprintf("20.10: Problem 6a: The probability that each server is busy is\u001b[36m %s \u001b[0m\u001b[31m* \u001b[0m\n", Pi0bar)) # TODO: Why is it PI0 not 1-PI0?


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

Ltau = sum(Lbar)

cat(sprintf("20.10: Problem 6b: The expected number of jobs in the system is\u001b[36m %f \u001b[0mjobs\n", Ltau))

#-----
# c)
#-----
Wtau = Ltau/sum(rbar)

Wtau1 = 0

for (i in 1:3) {
  Wtau1 = Wtau1 + Lbar[i]/lambdabar[i]
}

cat(sprintf("20.10: Problem 6c: The average time a job spends in the system is\u001b[36m %f \u001b[0mhours or\u001b[36m %f \u001b[0mminutes\n", Wtau, Wtau*60))
cat(sprintf("       or is it\u001b[36m %f \u001b[0mhours or\u001b[36m %f \u001b[0mminutes\u001b[0m\u001b[31m* \u001b[0m\n\n", Wtau1, Wtau1*60))
# FIXME: Wtau or Wtau1, which is best? I think W1 as its dr jacobs