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
# PROBLEM 6 # TODO: clean up this code after we understand it
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

get_lambdabar10 = function(P, rbar) {
    return(
        solve(diag(ncol(P))-t(P))%*%rbar
    )
}
lambdabar = get_lambdabar10(P, rbar)

#-----
# a)
#-----
# so each each queue is an M/M/1/GD/inf/inf queue (20.4). So any queue specific things like Pi0 etc which is what we have to do here
PI0bar = c()
for (i in 1:3) {
  rho = lambdabar[i]/mubar[i]
  PI0bar[i] = 1-rho
}
cat(sprintf("20.10: Problem 6a: The probability that each server is busy is\u001b[36m %s \u001b[0m\u001b[31m* \u001b[0m\n", PI0bar)) # TODO: Why is it PI0 not 1-PI0?


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


#--------------------------------------------------
# PROBLEM 2 (not from tut) # TODO: clean up
#--------------------------------------------------
# Consider an automobile assembly line in which each car
# undergoes two types of service: painting, then engine
# installation. Each hour, an average of 22.4 unpainted chassis
# arrive at the assembly line. It takes an average of 2.4 minutes
# to paint a car and an average of 3.75 minutes to install an
# engine. The assembly line has one painter and two engine
# installers. Assume that interarrival times and service times
# are exponential.
#     (a) On the average, how many painted cars without com-
#     pletely installed engines will be in the facility?
#     (b) On the average, how long will a painted car have to
#     wait before installation of its engine begins?

rbar = c(
    22.4,
    0
) # c/h
mubar = c(
    1/2.4*60,
    1/3.75*60
) # c/h
sbar = c(
    1,
    2
)
P = matrix(c(
  0,   1,
  0, 0
), nrow=2, byrow=TRUE)

lambdabar = get_lambdabar10(P, rbar) # TODO: lambdabar is constant. is this because there is no backtracking. THis makes sense as it is 22.4

#-------above this line seems correct. Now what I want to calculate is L(station2)

# S2 is a 20.6 Q
L6 = function(lambda, mu, s) {
    return(lambda*W6(lambda, mu, s))
}

cat(sprintf("20.10: Problem 2a: On average there will be\u001b[36m %f \u001b[0mpainted cars without fully installed engines\n", L6(lambdabar[2], mubar[2], sbar[2])))

Wq6 = function(lambda, mu, s) {
    (Pj6(lambda, mu, s)*rho6(lambda, mu, s))/( (1-rho6(lambda, mu, s))*lambda )
}

cat(sprintf("20.10: Problem 2b: On average a painted car will have to wait\u001b[36m %f \u001b[0mhours before instilation begins\n\n", Wq6(lambdabar[2], mubar[2], sbar[2])))

#--------------------------------------------------
# PROBLEM 4 (not from tut)
#--------------------------------------------------
# An average of 120 students arrive each hour (interarrival
# times are exponential) at State College’s Registrar’s Office
# to change their course registrations. To complete this
# process, a person must pass through three stations. Each
# station consists of a single server. Service times at each
# station are exponential, with the following mean times:
# station 1, 20 seconds; station 2, 15 seconds; station 3, 12
# seconds.
#   (a) On the average, how many students will be present
#   in the registrar’s office for changing courses?

# I will assume that this is exponential series Q with no backtrack fron the question
lambda = 120 # c/h, M, NOTE: we don't have to calc lambdabar as this is series without backtrack
s = 1
mubar = c(
    1/20*60^2, # c/h, M
    1/15*60^2, # c/h, M
    1/12*60^2 # c/h, M
) # M

# we want to find sum(L1, L2, L3)
Lbar = c()
for (i in 1:3) {
    Lbar[i] = L6(lambda, mubar[i], s)
}
cat(sprintf("20.10: Problem 4: On average there will be\u001b[36m %f \u001b[0mstudents in the registrar's office\n\n", sum(Lbar)))

# TODO: since we know 20.6 well and we will have our functions. Can we just uses 20.6 equations with s=1 in place of 20.4?

#--------------------------------------------------
# TT07
#--------------------------------------------------
# Consider an open queueing network consisting of three
# workstations linked together with probabilities of flow among
# them as shown in the table below.

#  |  1       2       3
# -|---------------------
# 1|  0       1       0
# 2|  0.2     0       0.8
# 3|  0.05    0.05    0

# Costomers arrive from outside to the system at a rate of 4
# customers per minute to workstation (1) and 3 per minute to
# workstation (2). Workstations (1) and (3) take on average
# 7 1/2(7.5?) seconds to serve a customer and workstation (2)
# takes on average 6 seconds to serve a customer. All
# interarrival times and service times are exponenrial distributed.

rbar = c(
    4, # c/m, M
    3, # c/m, M
    0  # c/m, M
)
mubar = c(
    1/7.5*60, # c/m, M
    1/6.0*60, # c/m, M
    1/7.5*60  # c/m, M
)
P = matrix(
    c(
        0, 1, 0,
        0.2, 0, 0.8,
        0.05, 0.05, 0
    ), byrow=TRUE, nrow=3
)
lambdabar = get_lambdabar10(P, rbar)

# I'm going to assume M/M/1/GD/∞/∞ for all the individual queues
s = 1

#------- Workstations 2 and 3 are equally as busy? PI0 compare?
cat(sprintf("TT07: Problem 1: PI0(WS2) = %f; PI0(WS3) = %f. Workstations 2 and 3 are\u001b[36m equally as busy \u001b[0m\n", 
PI06(lambdabar[2], mubar[2], s), PI06(lambdabar[3], mubar[3], s)))

#------- There are an average of ? customers in the system
Lbar = c()
for (i in 1:3) {
    Lbar[i] = L6(lambdabar[i], mubar[i], s)
}
cat(sprintf("TT07: Problem 2: There are an average of\u001b[36m %f \u001b[0mcustomers in the system\n", sum(Lbar)))

#------- Average time at (1) per visit?
cat(sprintf("TT07: Problem 3: The average time a customer spends at station (1) per visit is\u001b[36m %f \u001b[0mminutes\n", W6(lambdabar[1], mubar[1], s)))

#------- Total time in the system?
Wtau = sum(Lbar)/sum(rbar)
cat(sprintf("TT07: Problem 4: The average total time a customer spends in the system is\u001b[36m %f \u001b[0mminutes\n\n", Wtau))


#------- Service rate of (2)?
# cat(sprintf("TT07: Problem 4: The service rate of station (2) is\u001b[31m ? \u001b[0m\n\n")) # TODO: how do we mesure service times? mubar[i]?

cat(sprintf("Open queues should be revisited. The functions and all are easy enough with the theory being well enough understood.
The problem is we didn't really code enough of it by hand. These parts are critical and are not on the formula sheet.
But honestly the main part is lambdabar and if we can just derive that well be fine, plus maybe it is a theory question.
Other than that maybe a robust question set on open queues to fully exaust the question set, like \"service rate\"\n", Wtau))