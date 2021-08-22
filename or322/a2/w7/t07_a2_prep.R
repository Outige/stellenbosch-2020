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
#   - ?

#--------------------------------------------------
# PROBLEM 1
#--------------------------------------------------
# A Social Security Administration branch is considering
# the following two options for processing applications for
# social security cards:

# OPTION 1:
# Three clerks process applications in parallel from
# a single queue. Each clerk fills out the form for the appli-
# cation in the presence of the applicant. Processing time is
# exponential with a mean of 15 minutes. Interarrival times
# are exponential.

# OPTION 2:
# Each applicant first fills out an application with-
# out the clerk’s help. The time to accomplish this is expo-
# nentially distributed, with a mean of 65 minutes. When the
# applicant has filled out the form, he or she joins a single line
# to wait for one of the three clerks to check the form. It takes
# a clerk an average of 4 minutes (exponentially distributed)
# to review an application.
# The interarrival time of applicants is exponential, and an
# average of 4.8 applicants arrive each hour
#   a) Which option will get applicants out of the office more quickly?