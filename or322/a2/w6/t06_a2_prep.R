#----------------------------------------------------------------------------------------------------
# SECTION 20.6
#----------------------------------------------------------------------------------------------------
# DETAILS:
#   start page: 1087
#   question page: 1094
#   tut questions: 2, 4, 10, 11
#   solutions: 1397
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
#--------------------------------------------------
# A small bank is trying to determine how many tellers to
# employ. The total cost of employing a teller is $100 per day,
# and a teller can serve an average of 60 customers per day.
# An average of 50 customers per day arrive at the bank, and
# both service times and interarrival times are exponential. If
# the delay cost per customer-day is $100, how many tellers
# should the bank hire?

# Q: M/M/s/GD/inf/inf
teller_cost = 100 # $/d
mu = 60 # c/d
lambda = 50 # c/d
delay_cost = 100 # $/c

rho = function(lambda, mu, s) {
    return(lambda/(mu*s))
}

PI0 = function(lambda, mu, s) {
    D = 0
    for (j in 0:(s-1)) {
        D = D + (s*rho(lambda, mu, s))^j/factorial(j)
    }
    D = D + (s*rho(lambda, mu, s))^s/(factorial(s)*(1-rho(lambda, mu, s)))
    return(1/D)
}

Pj = function(lambda, mu, s) {
    return(
        PI0(lambda, mu, s) * ( (s*rho(lambda, mu, s) )^s  /  ( factorial(s)*(1-rho(lambda, mu, s))) )
    )
}

Lq = function(lambda, mu, s) {
    return(
        ( ( Pj(lambda, mu, s))*rho(lambda, mu, s) ) / (1-rho(lambda, mu, s))
    )
}

Wq = function(lambda, mu, s) {
    return(Lq(lambda, mu, s)/lambda)
}

cost = function(lambda, mu, s, teller_cost, delay_cost) {
    s*teller_cost + lambda*delay_cost*Wq(lambda, mu, s)
}

ans = c(2^31-1, -1)
    # ans[1]: min cost encountered (set to max int to start)
    # ans[2]: the amount of tellers where the min cost was encountered (set to 0 to start)
for (s in 1:10) {
    c = cost(lambda, mu, s, teller_cost, delay_cost)
    if (c < ans[1]) {
        ans[1] = c
        ans[2] = s
    }
}

cat(sprintf("20.6: Problem 2: The optimal number of tellers is \u001b[36m %d \u001b[0m at a min cost of \u001b[36m $%.2f \u001b[0m per day\n\n", ans[2], ans[1]))


#--------------------------------------------------
# PROBLEM 4
#--------------------------------------------------
# MacBurger’s is attempting to determine how many
# servers (or lines) should be available during the breakfast
# shift. During each hour, an average of 100 customers arrive
# at the restaurant. Each line or server can handle an average
# of 50 customers per hour. A server costs $5 per hour, and
# the cost of a customer waiting in line for 1 hour is $20.
# Assuming that an M/M/s/GD/∞/∞ model is applicable,
# determine the number of lines that minimizes the sum of
# delay and service costs.

# Q: M/M/s/GD/∞/∞
lambda = 100 # c/h
mu = 50 # c/h
teller_cost = 5 # $/h
delay_cost = 20 # $/h
# Looking for s to minimize cost

for (s in 2:100) {
    c = cost(lambda, mu, s, teller_cost, delay_cost)
    if (is.finite(c) & c < ans[1]) {
        ans[1] = c
        ans[2] = s
    }
}

cat(sprintf("20.6: Problem 4: The optimal number of tellers is \u001b[36m %d \u001b[0m at a min cost of \u001b[36m $%.2f \u001b[0m per day\n\n", ans[2], ans[1]))


#--------------------------------------------------
# PROBLEM 10
#--------------------------------------------------
# A data storage system consists of 3 disk drives sharing
# a common queue. An average of 50 storage requests arrive
# per second. The average time required to service a request
# is .03 second. Assuming that interarrival times and service
# times are exponential, determine:
#     a) The probability that a given disk drive is busy
#     b) The probability that no disk drives are busy
#     c) The probability that a job will have to wait
#     d) The average number of jobs present in the storage
#     system

lambda = 50 # c/s
mu = 1/0.03 # c/s
s = 3

PIj = function(lambda, mu, s, j) {
    if (j < s) {
        return(
            ((s*rho(lambda, mu, s))^j*PI0(lambda, mu, s)) / factorial(j)
        )
    } else {
        return(
            ((s*rho(lambda, mu, s))^j*PI0(lambda, mu, s)) / (factorial(s)*s^(j-s))
        )
    }
}

ans = 0
for (j in 1:3) {
    ans = ans + j/3*PIj(lambda, mu, s, j)
}
cat(sprintf("20.6: Problem 10a: The probability that a given disk drive is busy is \u001b[36m %.4f \u001b[0m\n", ans)) # TODO: this is uncertain

cat(sprintf("20.6: Problem 10b: The probability that no disk drives are busy is \u001b[36m %.4f \u001b[0m\n", PI0(lambda, mu, s)))

cat(sprintf("20.6: Problem 10c: The probability that a job will have to wait is \u001b[36m %.4f \u001b[0m\n", Pj(lambda, mu, s)))

Ls = function(lambda, mu, s) {
    return(lambda/(mu))
}

L = function(lambda, mu, s) {
    return(Lq(lambda, mu, s) + Ls(lambda, mu, s))
}
cat(sprintf("20.6: Problem 10d: The average number of jobs present in the storage system is \u001b[36m %.4f \u001b[0m\n\n", L(lambda, mu, s)))

#--------------------------------------------------
# PROBLEM 11
#--------------------------------------------------
# A Northwest Airlines ticket counter forecasts that 200
# people per hour will need to check in. It takes an average of
# two minutes to service a customer. Assume that interarrival
# times and service times are exponential and that all
# customers wait in a single line for the first available agent.
#     a) If we want the average time a customer spends in
#     line and in service to be 30 minutes or less, how many
#     ticket agents should be on duty?
#     b) If we want 95% of all customers to wait 45 minutes
#     or less in line, how many ticket agents should be on
#     duty?

lambda = 200 # c/h
mu = 1/2*60 # c/h

# a) looking for W to be 30 mins or less
W = function(lambda, mu, s) {
    # return(
    #     Pj(lambda, mu, s)/(s*mu - lambda) + 1/mu
    # )
    return(L(lambda, mu, s)/lambda)
}
ans = -1
for (s in 1:11) {
    # print(W(lambda, mu, s))
    if (W(lambda, mu, s) <= 0.5) {
        ans = s
        break
    }
}
cat(sprintf("20.6: Problem 11a: Require \u001b[36m %d \u001b[0m tellers for a wait time of 30 mins or less\n", ans)) # TODO: Can't seem to get this one. All my W seem to be less than 0.5. They even start out negative

PWq = function(lambda, mu, s, t) {
    return(
        min(Pj(lambda, mu, s)*exp(-s*mu*(1-rho(lambda, mu, s))*t), 1)
    )
}

ans = -1
for (s in 1:20) {
    if (PWq(lambda, mu, s, 45/60) < 0.05) {
        ans = s
        break;
    }
}
cat(sprintf("20.6: Problem 11b: If we want \u001b[36m %f \u001b[0m of all customers to wait 45 minutes or less, we need \u001b[36m %d \u001b[0m tellers\n\n", 1-PWq(lambda, mu, ans, 45/60), ans)) # NOTE: My result seems to match up to the other answers but I still think this is a scuffed question

#--------------------------------------------------
# FINAL REMARKS
#--------------------------------------------------
# TODO: The 2 P(W>t) functions. When to use one over the other etc
# TODO: TT06




#----------------------------------------------------------------------------------------------------
# SECTION 20.7
#----------------------------------------------------------------------------------------------------
# DETAILS:
#   start page: 1095
#   question page: 1096
#   tut questions: 2, 3
#   solutions: 1397
#   Q: M/(G/M)/∞/GD/∞/∞

# SUMMARY:
#   - this section deals with queues that don't necesseraly have exponentially distrabuted service times, which results in the no memory property not being applicable.
#   - the other standout feature is that there are ∞ servers. think self serving queue and university application. no Wq


#--------------------------------------------------
# PROBLEM 2
#--------------------------------------------------
# The State U doctoral program in business admits an
# average of 25 doctoral students each year. If a doctoral
# student spends an average of 4 years in residence at State
# U, how many doctoral students would one expect to find
# there?

lambda = 25 # c/y
mu = 1/4 # y/c

L = function(lambda, mu) {
    return(lambda/mu)
}

cat(sprintf("20.7: Problem 2: We can expect to find \u001b[36m %f \u001b[0m doctoral students\n\n", L(lambda, mu)))


#--------------------------------------------------
# PROBLEM 3
#--------------------------------------------------
# There are at present 40 solar energy construction firms
# in the state of Indiana. An average of 20 solar energy
# construction firms open each year in the state. The average
# firm stays in business for 10 years.
#     a) If present trends continue,
#     what is the expected number of solar energy construction
#     firms that will be found in Indiana?
#     b) If the time between the
#     entries of firms into the industry is exponentially distributed,
#     what is the probability that (in the steady state) there will
#     be more than 300 solar energy firms in business? (Hint: For
#     large l, the Poisson distribution can be approximated by a
#     normal distribution.)

lambda0 = 40 # number of firms at the start
lambda = 20 # c/y
mu = 1/10 # c/y

cat(sprintf("20.7: Problem 3a: The expected number of solar energy construction firms that will be found in Indiana is \u001b[36m %f \u001b[0m\n", L(lambda, mu)+lambda0))

PIj = function(lambda, mu, j) {
    return(
        ( ((lambda/mu)^j)*exp(-lambda/mu) ) / factorial(j)
    )
}

# we are looking for PIj(j >= 300) (keep in mind lambda0 = 40) => PIj(j > 260) = 1-PIj(j <= 260)
sigmaPIj = 0
for (j in 1:260) {
    c = PIj(lambda, mu, j)
    if (is.finite(c)) { # we get NaN values
        sigmaPIj = sigmaPIj + c
    }
}
cat(sprintf("20.7: Problem 3b: The steady state probability that there will be more than 300 firms is \u001b[36m %.7f ~= 1 \u001b[0m\n\n", 1-sigmaPIj)) # TODO: this answer is not mathcing up to the other students. However I believe the method and resoning are both sound. Accourding to the book the probability is 0 like the other students say


#--------------------------------------------------
# FINAL REMARKS
#--------------------------------------------------
# TODO: TT06
# mostly easy section


#----------------------------------------------------------------------------------------------------
# SECTION 20.9
#----------------------------------------------------------------------------------------------------
# DETAILS:
#   start page: 1099
#   question page: 1103
#   tut questions: 2, 3, 10
#   solutions: 1398
#   Q: M/M/1/GD/K/∞

# SUMMARY:
#   - finite source models
#   - finite population of customers


#--------------------------------------------------
# PROBLEM 2
#--------------------------------------------------
# My dog just had 3 frisky puppies who jump in and out
# of their whelping box. A puppy spends an average of 10
# minutes (exponentially distributed) in the whelping box
# before jumping out. Once out of the box, a puppy spends an
# average of 15 minutes (exponentially distributed) before
# jumping back into the box.
#     a) At any given time, what is the probability that more
#     puppies will be out of the box than will be in the box?
#     b) On the average, how many puppies will be in the
#     box?

mu = 1/10 # c/m
lambda = 1/15 # c/m
K = 3
# Q: M/M/3/GD/3/3

# we are looking for PIj(j < c/2) => PI0 + PI1
rho = function(lambda, mu) {
    return(lambda/mu)
}

PI0 = function(lambda, mu, K) {
    D = 0
    for (j in 0:K) {
        D = D + choose(K, j)*(lambda/mu)^j
    }
    return(1/D)
}

PIj = function(lambda, mu, K, R, j) {
    if (j <= R) {
        return(
            choose(K, j)*rho(lambda, mu)^j*PI0(lambda, mu, K)
        )
    } else { # R < j
        return(
            ( choose(K, j)*rho(lambda, mu)^j*factorial(j)*PI0(lambda, mu, K) ) / ( factorial(R)*R^(j-R) )
        )
    }
}

cat(sprintf("20.9: Problem 3a: The probability that more puppies will be out of the box is \u001b[36m %f \u001b[0m\n", 
PIj(lambda, mu, K, K, 1) + PI0(lambda, mu, K)))


L = function(lambda, mu, K, R) {
    sigma = 0
    for (j in 0:K) {
        sigma = sigma + j*PIj(lambda, mu, K, R, j)
    }
    return(sigma)
}
cat(sprintf("20.9: Problem 3b: On average,there will be \u001b[36m %f \u001b[0m puppies in the box\n\n", L(lambda, mu, K, K)))


#--------------------------------------------------
# PROBLEM 3
#--------------------------------------------------
# Gotham City has 10,000 streetlights. City investigators
# have determined that at any given time, an average of 1,000
# lights are burned out. A streetlight burns out after an average
# of 100 days of use. The city has hired Mafia, Inc., to replace
# burned-out lamps. Mafia, Inc.’s contract states that the
# company is supposed to replace a burned-out street lamp in
# an average of 7 days. Do you think that Mafia, Inc. is living
# up to the contract?

K = 10000
lambda = 1/100 # c/d - A streetlight burns out after an average of 100 days of use
mu = 1/7 # c/d
R = 1 # R is the max number of servers - 1 mafia company
# Q: M/M/1/GD/10k/10k

# I couldn't figure out this question at all. "memo" assisted
# rho = function(lambda, mu) {
#     return(lambda/mu)
# }

# PI0 = function(lambda, mu, K) {
#     D = 0
#     for (j in 0:K) {
#         D = D + choose(K, j)*(lambda/mu)^j
#     }
#     return(1/D)
# }

# PIj = function(lambda, mu, K, R, j) {
#     if (j <= R) {
#         return(
#             choose(K, j)*rho(lambda, mu)^j*PI0(lambda, mu, K)
#         )
#     } else { # R < j
#         return(
#             ( choose(K, j)*rho(lambda, mu)^j*factorial(j)*PI0(lambda, mu, K) ) / ( factorial(R)*R^(j-R) )
#         )
#     }
# }

# L = function(lambda, mu, K, R) {
#     L = 0
#     for (j in 0:K) {
#         L = L + j*PIj(lambda, mu, K, R, j)
#     }
#     return(L)
# }
# PI0(lambda, mu, K)
# L(lambda, mu, 100, R)

lambda = 1/100 # c/d
mu = 1/7 # c/d supposed mu
rho = lambda/mu
K = 10000 #?

Pi0 = function(lamnda, mu, K) {
  D = 0
  for (K in 0:K) {
    D = D + choose(K, j)*(lambda/mu)^j
  }
  return(1/D)
}
Pi0(lambda, mu, K)

Pij = function(lambda, mu, K, j) {
  choose(K, j)*(lambda/mu)^j*Pi0(lambda, mu, K)
}
Pij(lambda, mu, K, 1)

L = 0
for (j in 0:K) {
  L = L + j*Pij(lambda, mu, K, j)
}
L