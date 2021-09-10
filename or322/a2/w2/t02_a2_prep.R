#----------------------------------------------------------------------------------------------------
# SECTION 17.4
#----------------------------------------------------------------------------------------------------

#--------------------------------------------------
# PROBLEM 1
#--------------------------------------------------
# In Example 1, what is the period of states 1 and 3?

states=c(0, 1, 2, 3, 4)
p = 0.5 # randomly chosen by me
P = matrix(c(
    1, 0, 0, 0, 0,
    1-p, 0, p, 0, 0,
    0, 1-p, 0, p, 0,
    0, 0, 1-p, 0, p,
    0, 0, 0, 0, 1
), nrow=5, byrow=TRUE) # GIVEN
rownames(P) = colnames(P) = states
cat(sprintf("17.4: Problem 1: Even if this chain didn't have exhaustive states, it still seems like (1) and (3) are aperiodic\u001b[36m\u001b[0m\u001b[31m* \u001b[0m\n\n"))

#--------------------------------------------------
# PROBLEM 3
#--------------------------------------------------
#   (a) Which states are transient?
#   (b) Which states are recurrent?
#   (c) Identify all closed sets of states.
#   (d) Is this chain ergodic?

P = matrix(c( # GIVEN
    0.0, 0.0, 1.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
    0.0, 0.0, 0.0, 0.0, 1.0, 0.0,
    1/4, 1/4, 0.0, 1/2, 0.0, 0.0,
    1.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 1/3, 0.0, 0.0, 0.0, 2/3
), nrow=6, byrow=TRUE)
rownames(P) = colnames(P) = c("a", "b", "c", "d", "e", "f")

cat(sprintf("17.4: Problem 3a: Transient states:\u001b[36m {d} \u001b[0m\n"))
cat(sprintf("17.4: Problem 3b: Transient states:\u001b[36m {a, b, c, e, f} \u001b[0m\n"))
cat(sprintf("17.4: Problem 3c: Closed sets:\u001b[36m {a, c, e}, {b, f} \u001b[0m\n"))
cat(sprintf("17.4: Problem 3d:\u001b[36m Not ergodic as d is transient \u001b[0m\n\n"))

#--------------------------------------------------
# PROBLEM 4
#--------------------------------------------------
# For each of the following chains(P1, P2), determine whether the
# Markov chain is ergodic. Also, for each chain, determine
# the recurrent, transient, and absorbing states.

# P1 given
P1 = matrix(c(
    0.0, 0.8, 0.2,
    0.3, 0.7, 0.0,
    0.4, 0.5, 0.1
), nrow=3, byrow=TRUE)
rownames(P1) = colnames(P1) = c("a", "b", "c")

# P2 given
P2 = matrix(c(
    0.2, 0.8, 0.0, 0.0,
    0.0, 0.0, 0.9, 0.1,
    0.4, 0.5, 0.1, 0.0,
    0.0, 0.0, 0.0, 1.0
), nrow=4, byrow=TRUE)
rownames(P2) = colnames(P2) = c("a", "b", "c", "d")

cat(sprintf("17.4: Problem 4:\nP1:\n\u001b[36mabsorbing states: {}, recurring states: {a, b, c}, transient states: {}; ergodic: TRUE\u001b[0m\n"))
cat(sprintf("P2:\n\u001b[36mabsorbing states: {d}, recurring states: {}, transient states: {a, b, c}; ergodic: FALSE\u001b[0m\n\n"))


#----------------------------------------------------------------------------------------------------
# SECTION 17.5
#----------------------------------------------------------------------------------------------------

#--------------------------------------------------
# PROBLEM 4
#--------------------------------------------------
# At the beginning of each year, my car is in good, fair, or
# broken-down condition. A good car will be good at the
# beginning of next year with probability .85; fair with
# probability .10; or broken-down with probability .05. A fair
# car will be fair at the beginning of the next year with
# probability .70 or broken-down with probability .30. It costs
# $6,000 to purchase a good car; a fair car can be traded in
# for $2,000; and a broken-down car has no trade-in value and
# must immediately be replaced by a good car. It costs $1,000
# per year to operate a good car and $1,500 to operate a fair
# car. Should I replace my car as soon as it becomes a fair car,
# or should I drive my car until it breaks down? Assume that
# the cost of operating a car during a year depends on the type
# of car on hand at the beginning of the year (after a new car,
# if any, arrives).
P = matrix(c(
    0.85, 0.10, 0.05,
    0.00, 0.70, 0.30,
    1.00, 0.00, 0.00
), nrow=3, byrow=TRUE)
rownames(P) = colnames(P) = c('G', 'F', 'B')

# Steady state distribution
get_PIj17 = function(P) {
    n = nrow(P)
    Pmod = P-diag(n)
    Pmod[1:n, 1] = 1
    PIj = c(1, rep(0, n-1))%*%solve(Pmod)
    return(PIj)
}

Pmod = P-diag(nrow(P))
Pmod[,1] = 1
Pmod = solve(Pmod)
PIj_keep = get_PIj17(P) #c(1,rep(0, nrow(P)-1))%*%Pmod

cost_keep = c(
    1000, # just costs $1000 to drive per year
    6000-2000+1000, # cost of good car - trade in value + cost to drive a good car that year
    6000  # cost to trade in for a good car
)
cost_keep = PIj_keep%*%cost_keep

P = matrix(c(
    0.85, 0.10, 0.05,
    1.00, 0.00, 0.00,
    1.00, 0.00, 0.00
), nrow=3, byrow=TRUE)

# Steady state distribution
Pmod = P-diag(nrow(P))
Pmod[,1] = 1
Pmod = solve(Pmod)
PIj_trade = get_PIj17(P)#c(1,rep(0, nrow(P)-1))%*%Pmod
cost_trade = c(
    1000, # just costs $1000 to drive per year
    1500, # costs $1500 to drive per year
    6000  # cost to trade in for a good car
)
cost_trade = PIj_trade%*%cost_trade

cat(sprintf("17.5: Problem 4: It costs \u001b[36m$%.2f\u001b[0m more to keep than to trade\u001b[0m\u001b[31m*\u001b[0m\n\n", cost_keep-cost_trade))


#--------------------------------------------------
# PROBLEM 7
#--------------------------------------------------
# Consider two stocks. Stock 1 always sells for $10 or
# $20. If stock 1 is selling for $10 today, there is a .80 chance
# that it will sell for $10 tomorrow. If it is selling for $20
# today, there is a .90 chance that it will sell for $20 tomorrow.
# Stock 2 always sells for $10 or $25. If stock 2 sells today
# for $10, there is a .90 chance that it will sell tomorrow for
# $10. If it sells today for $25, there is a .85 chance that it
# will sell tomorrow for $25.
#       (a) On the average, which stock will sell for a higher price?
#       (b) Find and interpret all mean first passage times.

P1 = matrix(c(
    0.80, 0.20,
    0.10, 0.90
), nrow=2, byrow=TRUE)
rownames(P1) = colnames(P1) = c('$10', '$20')
PIj1 = get_PIj17(P1)
av1 = PIj1%*%c(10,20)

P2 = matrix(c(
    0.90, 0.10,
    0.15, 0.85
), nrow=2, byrow=TRUE)
rownames(P2) = colnames(P2) = c('$10', '$25')
PIj2 = get_PIj17(P2)
av2 = PIj2%*%c(10, 25)

cat(sprintf("17.5: Problem 7a: On average stock 1 sells for \u001b[36m$%.2f\u001b[0m more than stock 2\u001b[0m\n", av1-av2))

cat(sprintf("17.5: Problem 7b: m11(p1): \u001b[36m%.2f\u001b[0m, m22(p1): \u001b[36m%.2f\u001b[0m, m11(p2): \u001b[36m%.2f\u001b[0m, m22(p2): \u001b[36m%.2f\u001b[0m, ", 1/PIj1[1], 1/PIj1[2], 1/PIj2[1], 1/PIj2[2]))
cat(sprintf("\u001b[0m\u001b[31mNeed to research mean first passage time\u001b[0m\n\n"))


#--------------------------------------------------
# PROBLEM 9
#--------------------------------------------------
# Two types of squirrels—gray and black—have been seen
# in Pine Valley. At the beginning of each year, we determine
# which of the following is true:
# There are only gray squirrels in Pine Valley.
# There are only black squirrels in Pine Valley.
# There are both gray and black squirrels in Pine Valley.
# There are no squirrels in Pine Valley.
# Over the course of many years, the following transition
# matrix has been estimated.
#         (a) During what fraction of years will gray squirrels be
#         living in Pine Valley?
#         (b) During what fraction of years will black squirrels be
#         living in Pine Valley?

P = matrix(c( # P matrix was given in the question
    0.70, 0.20, 0.05, 0.05,
    0.20, 0.60, 0.10, 0.10,
    0.10, 0.10, 0.80, 0.00,
    0.05, 0.05, 0.10, 0.80
), nrow=4, byrow=TRUE)
rownames(P) = colnames(P) = c("Gray", "Black", "Both", "Neither")
PIj = get_PIj17(P)

cat(sprintf("17.5: Problem 9a: The fraction of years gray squirils live in pine valley is \u001b[36m%f\u001b[0m\n", PIj[1]+PIj[3]))
cat(sprintf("17.5: Problem 9b: The fraction of years black squirils live in pine valley is \u001b[36m%f\u001b[0m\n\n", PIj[2]+PIj[3]))



#--------------------------------------------------
# PROBLEM 10
#--------------------------------------------------
# Payoff Insurance Company charges a customer
# according to his or her accident history. A customer who
# has had no accident during the last two years is charged a
# $100 annual premium. Any customer who has had an
# accident during each of the last two years is charged a $400
# annual premium. A customer who has had an accident during
# only one of the last two years is charged an annual premium
# of $300. A customer who has had an accident during the last
# year has a 10% chance of having an accident during the
# current year. If a customer has not had an accident during
# the last year, there is only a 3% chance that he or she will
# have an accident during the current year
#       (a) During a given year, what is the average premium paid by a Payoff
#       customer? (Hint: In case of difficulty, try a four-state Markov
#       chain.)

# METHOD 1
P = matrix(c(
    0.97, 0.03,
    0.90, 0.10
), nrow=2, byrow=TRUE)
colnames(P) = rownames(P) = c("N", "A")
PIj = get_PIj17(P)
payoff = PIj%*%c(100, 400)

# METHOD 2
P = matrix(c(
    0.97, 0.03, 0.00, 0.00,
    0.00, 0.00, 0.10, 0.90,
    0.00, 0.00, 0.10, 0.90,
    0.97, 0.03, 0.00, 0.00
), nrow=4, byrow=TRUE)
colnames(P) = rownames(P) = c("NN", "NA", "AA", "AN")
PIj = get_PIj17(P)
payoff = PIj%*%c(100, 400, 400, 400)

cat(sprintf("17.5: Problem 10: Payoff will expect to pay \u001b[36m$%f\u001b[0m\u001b[31m(good one for Sam)\u001b[0m\n\n", payoff))



#----------------------------------------------------------------------------------------------------
# TT02
#----------------------------------------------------------------------------------------------------
# The birders in Stellenbosch have been observing and recording the presence of gray kestrels and
# red kestrels in the town and vicinity, every spring time, over many years. The observation states of
# kestrels in spring time in Stellenbosch can be summarized as
# 1) only gray kestrels observed or
# 2) only red kestrels observed or
# 3) both red and gray kestrels observed or
# 4) both red and gray kestrels are absent.
# From past records the probabilities in which these different observation states seemed to follow up
# on each other were found, as shown in the table below. Model the situation as a Markov chain and select
# the correct statements form the list below. The selection of incorrect statements will lead to the deduction of earned marks.

P = matrix(c(
    0.20, 0.40, 0.10, 0.30,
    0.30, 0.30, 0.20, 0.20,
    0.10, 0.20, 0.30, 0.40,
    0.25, 0.35, 0.30, 0.10
), byrow=TRUE, nrow=4)
colnames(P) = rownames(P) = c("G", "R", "GR", "N")


P2 = P%*%P


cat(sprintf("TT02: Problem 1: Say in spring time 2021, both gray- and red kestrels are observed in Stellenbosch,  the probability of both gray- and red kestrels being absent in spring time 2023 then is \u001b[36m%f\u001b[0m(0.230)\n", P2[3,4]))


Pmod = P - diag(4)
Pmod[,1] = 1
PIj = c(1,0,0,0)%*%solve(Pmod)
cat(sprintf("TT02: Problem 2: The fraction of spring times  in which only gray kestrels will be observed in Stellenbosch is \u001b[36m%f\u001b[0m(0.2208472)\n", PIj[1]))

P3 = P2%*%P2
cat(sprintf("TT02: Problem 3: Say in spring time 2021, both gray- and red kestrels are observed in Stellenbosch,  the probability of both gray- and red kestrels being absent in spring time 2024 then is \u001b[36m%f\u001b[0m(0.242150)\n", P3[3,4]))

M22 = 1/PIj[2]
cat(sprintf("TT02: Problem 4: Say in some spring time only red kestrels are observed, then on average \u001b[36m%f\u001b[0m(3.207634) spring times later, only red-kestrels will be observed again\n", M22))

cat(sprintf("TT02: Problem 5: Say in spring time 2021, both gray- and red kestrels are observed in Stellenbosch,  the probability of both gray- and red kestrels being absent in spring time 2022 then is \u001b[36m%f\u001b[0m(0.4)\n", P[3,4]))

cat(sprintf("TT02: Problem 6: The fraction of spring times  in which no gray kestrels will be observed in Stellenbosch is \u001b[36m%f\u001b[0m(0.5544979)\n\n", PIj[2] + PIj[4]))