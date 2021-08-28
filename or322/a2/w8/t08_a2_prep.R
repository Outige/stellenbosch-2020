#----------------------------------------------------------------------------------------------------
# SECTION 20.11
#----------------------------------------------------------------------------------------------------
# DETAILS:
#   start page: ?
#   question page: ?
#   tut questions: 1, 6, 8
#   solutions: ?
#   Q: M/G/s/GD/s/∞

# SUMMARY:
#   - no q only servers => Lq=Wq=0
#   - when servers are busy customers are lost
#   - service times not M

#--------------------------------------------------
# PROBLEM 1
#--------------------------------------------------
# Suppose that a fire department receives an average of 24
# requests for fire engines each hour. Each request causes a
# fire engine to be unavailable for an average of 20 minutes.
# To have at most a 1% chance of being unable to respond to
# a request, how many fire engines should the fire department
# have?

lambda = 24 # c/h
mu = 1/20*60 # c/h
# we want PIs11 to <= 0.01

rho11 = function(lambda, mu) {
    return(lambda/mu)
}

PIs11 = function(lambda, mu, s) {
    N = rho11(lambda, mu)^s/factorial(s)
    D = 0
    for (j in 0:s) {
        D = D + rho11(lambda, mu)^j/factorial(j)
    }
    return(N/D)
}

for (s in 1:1000) {
    if (PIs11(lambda, mu, s) <= 0.01) break
}
cat(sprintf("20.11: Problem 1: To have at most 0.01 refusal rate(rr) the fire dpt should have\u001b[36m %d \u001b[0mtrucks for an rr of\u001b[36m %f \u001b[0m\n\n", s, PIs11(lambda, mu, s)))


#--------------------------------------------------
# PROBLEM 6
#--------------------------------------------------
# (Requires the use of a spreadsheet or LINGO) US
# Airlines receives an average of 500 calls per hour from
# customers who want to make a reservation (time between
# calls follows an exponential distribution). It takes an average
# of 3 minutes to handle each call. Each customer who buys
# a ticket contributes $100 to US Airlines profit. It costs $15
# per hour to staff a telephone line. Any customer who receives
# a busy signal will purchase a ticket on another airline. How
# many telephone lines should US Airlines have?

lambda = 500 # c/h, M
mu = 1/3*60 # c/h
ticket_price = 100
teller_cost = 15

L11 = function(lambda, mu, s) {
    return(
        (lambda*(1-PIs11(lambda, mu, s)))/mu
    )
}

lambda11 = function(lambda, mu, s) {
    return(lambda*(1-PIs11(lambda, mu, s)))
}

profit11.6 = function(lambda, mu, s) {
    return(lambda11(lambda, mu, s)*ticket_price - teller_cost*s)
}

s = 1
for (j in 1:150) {
    if (profit11.6(lambda, mu, j) > profit11.6(lambda, mu, s)) {
        s = j
    }
}
cat(sprintf("20.11: Problem 6: In order to maximize profits the airline should employ\u001b[36m %d \u001b[0mtellers for a profit of\u001b[36m $%.2f \u001b[0m\n\n", s, profit11.6(lambda, mu, s)))


#--------------------------------------------------
# PROBLEM 6
#--------------------------------------------------
# A company’s warehouse can store up to 4 units of a
# good. Each month, an average of 10 orders for the good are
# received. The times between the receipt of successive orders
# are exponentially distributed. When an item is used to fill
# an order, a replacement item is immediately ordered, and it
# takes an average of one month for a replacement item to
# arrive. If no items are on hand when an order is received,
# the order is lost.
#   (a) What fraction of all orders will be lost due
#   to shortages? (Hint: Let the storage space for each item be
#   a server and think about what it means for a server to be
#   busy. Then come up with an appropriate definition of
#   “service” time.)

# NOTE: these types of questions can be confusing
# as they take a less logical model. however we 
# can use our understanding of queues to help us
# out. for example: "The times between the receipt of successive orders
# are exponentially distributed". If we are able to determine that
# this is a 20.11 Q as well then we know orders/hour is our arrival times
# as the queue is M/G, where service times are not M
lambda = 10 # customers/month, M
mu = 1 # customers/month
s = 4
cat(sprintf("20.11: Problem 8: The fraction of orders lost to shortages is\u001b[36m %f \u001b[0m\n\n", PIs11(lambda, mu, s)))

#----------------------------------------------------------------------------------------------------
# SECTION 20.13
#----------------------------------------------------------------------------------------------------
# DETAILS:
#   start page: ?
#   question page: ?
#   tut questions: ?
#   solutions: ?
#   Q: ???????M/G/s/GD/s/∞

# SUMMARY:
#   - constant number of jobs present
#   - workstations are linked
cat(sprintf("\u001b[31mCurrently attempting section 20.13. Be aware that this section is tik energy.\u001b[0m\n"))


#--------------------------------------------------
# PROBLEM 1
#--------------------------------------------------
# Jobs arrive to a file server consisting of a CPU and two
# disks (disk 1 and disk 2). With probability 13/20, a job goes
# from CPU to disk 1, and with probability 6/20, a job goes
# from CPU to disk 2. With probability 1/20, a job is finished
# after its CPU operation and is immediately replaced by
# another job. There are always 3 jobs in the system. The
# mean time to complete the CPU operation is .039 second.
# The mean time to complete the disk 1 operation is .18
# second, and the mean time to complete the disk 2 operation
# is .26 second.
#       (a) Determine the steady-state distribution of the num-
#       ber of jobs at each part of the system.
#       (b) What is the average number of jobs at CPU? Disk
#       1? Disk 2?
#       (c) What is the probability that CPU is busy? Disk 1?
#       Disk 2?
#       (d) What is the average number of jobs completed per
#       second by CPU? Disk 1? Disk 2?


#--------------
# STEP 1: write down all information from the question
#--------------
N = 3 # number of stations
mubar = c(
    1/0.039, # jobs/second (mu must be unit/time)
    1/0.180, # jobs/second
    1/0.260  # jobs/second

) # service times from the question
P = matrix(c(
    1/20, 13/20, 6/20,
    1, 0, 0,
    1, 0, 0
), nrow=3, byrow=TRUE) # transition probability martix from the question
rownames(P) = colnames(P) = c("CPU", "D1", "D2")
S = 3 # 3 jobs

#--------------
# STEP 2: calculate lambdabar (non-main stations need to be calculated)
#--------------
A = diag(3)-t(P) # transform P

LK = A[2:3,2:3] # leave out 1st column and row (the main column and row (of the main S)) # TODO: always I?

RK = -1*A[2:3,1] # negative main column, leaving out main index

lambda23=solve(LK)%*%RK # solve for non-main lambda: Inv(LK)*RK

lambdabar=c(1.0, lambda23) # main lambda is 1.0 with the rest relative



get_lambdabar13 = function(P) {
    N = ncol(P)
    A = diag(N)-t(P)
    LK = A[2:N,2:N]
    RK = -1*A[2:N,1]
    lambda_other = solve(LK)%*%RK
    return(c(1, lambda_other))
} # generic function of STEP 2, req main-col=1 and main-row=1 
lambdabar=get_lambdabar13(P)


#--------------
# STEP 3: calculate rhobar
#--------------
rhobar=lambdabar/mubar # lambdabar[i]/mubar[i]

#----------
# 6) create a vector with all the possible states (nij)
#----------
# install.packages("partitions") # you need to uncomment this if you have never installed the package before
library(partitions) # import function from newly imported package
all_states=t(compositions(N,S))
all_states