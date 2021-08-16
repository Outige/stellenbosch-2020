#----------------------------------------------------------------------------------------------------
# SECTION 20.6
#----------------------------------------------------------------------------------------------------
# DETAILS:
#   start page: 1087
#   question page: 1094
#   tut questions: 2, 4, 10, 11
#   Q: M/M/s/GD/inf/inf

# SUMMARY:
#   this section deals with queues like the melkbos PnP. parallel queues all doing the same job able to serve any customer

# TODO: kendall-lee notation:
#   1) M
#   2) M
#   3) s
#   4) GD
#   5) inf
#   6) inf

#--------------------------------------------------
# PROBLEM 2
# TODO: copy question from textbook
#--------------------------------------------------
# Q: M/M/s/GD/inf/inf
teller_cost = 100 # $/d
mu = 60 # c/d
lambda = 50 # c/d
delay_cost = 100 # $/c

delay_cost

rho = function(lambda, mu, s) {
    return(lambda/(mu*s))
}

cost = function(lambda, mu, s) {
    s*teller_cost + delay_cost*L(???)
}