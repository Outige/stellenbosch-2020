#----------------------------------------------------------------------------------------------------
# SECTION 17.2
#----------------------------------------------------------------------------------------------------
#--------------------------------------------------
# PROBLEM 2
#--------------------------------------------------
# cat(sprintf("20.10: Problem 6a: The probability that each server is busy is\u001b[36m %s \u001b[0m\u001b[31m* \u001b[0m\n", PI0bar)) # TODO: Why is it PI0 not 1-PI0?
# Consider an inventory system in which the sequence of
# events during each period is as follows. (1) We observe the
# inventory level (call it i) at the beginning of the period.
# (2) If i <= 1, 4 - i units are ordered. If i
# 2, 0 units are
# ordered. Delivery of all ordered units is immediate. (3) With
# probability 1/3 , 0 units are demanded during the period; with
# probability 1/3 , 1 unit is demanded during the period; and
# with probability 1/3 , 2 units are demanded during the period.
# (4) We observe the inventory level at the beginning of the
# next period.
# Define a period’s state to be the period’s beginning
# inventory level. Determine the transition matrix that could
# be used to model this inventory system as a Markov chain.

S = c(0, 1, 2, 3, 4) # Si is the amount of inventory at the start of a given day
P = matrix(c(
    0, 0, 1/3, 1/3, 1/3,
    0, 0, 1/3, 1/3, 1/3,
    1/3, 1/3, 1/3, 0, 0,
    0, 1/3, 1/3, 1/3, 0,
    0, 0, 1/3, 1/3, 1/3
), nrow=5, byrow=TRUE)
colnames(P)=rownames(P)=S

#--------------------------------------------------
# PROBLEM 4
#--------------------------------------------------
# Smalltown weather, as follows: (1) If the last two days have
# been sunny, then 95% of the time, tomorrow will be sunny.
# (2) If yesterday was cloudy and today is sunny, then 70% of
# the time, tomorrow will be sunny. (3) If yesterday was sunny
# and today is cloudy, then 60% of the time, tomorrow will
# be cloudy. (4) If the last two days have been cloudy, then
# 80% of the time, tomorrow will be cloudy.
# Using this information, model Smalltown’s weather as a
# Markov chain. If tomorrow’s weather depended on the last
# three days of Smalltown weather, how many states will be
# needed to model Smalltown’s weather as a Markov chain?

# SS -> SSS 0.95 # SS -> SSC 0.05
# CS -> CSS 0.70 # CS -> CSC 0.30
# SC -> SCC 0.60 # SC -> SCS 0.40
# CC -> CCC 0.80 # CC -> CCS 0.20

# FINALY
# SS -> SS 0.95 # SS -> SC 0.05
# CS -> SS 0.70 # CS -> SC 0.30
# SC -> CC 0.60 # SC -> CS 0.40
# CC -> CC 0.80 # CC -> CS 0.20
P = matrix(c(
    # SS  # SC  # CC  # CS
    0.95, 0.05, 0.00, 0.00, # SS
    0.00, 0.00, 0.60, 0.40, # SC
    0.00, 0.00, 0.80, 0.20, # CC
    0.70, 0.30, 0.00, 0.00  # CS
), nrow=4, byrow=TRUE)
rownames(P) = colnames(P) = c('SS', 'SC', 'CC', 'CS')

#--------------------------------------------------
# PROBLEM 6
#--------------------------------------------------
# TODO

#----------------------------------------------------------------------------------------------------
# SECTION 17.3
#----------------------------------------------------------------------------------------------------
#--------------------------------------------------
# PROBLEM 1
#--------------------------------------------------
# Each American family is classified as living in an urban,
# rural, or suburban location. During a given year, 15% of all
# urban families move to a suburban location, and 5% move
# to a rural location; also, 6% of all suburban families move
# to an urban location, and 4% move to a rural location;
# finally, 4% of all rural families move to an urban location,
# and 6% move to a suburban location.
#       (a) If a family now lives in an urban location, what is
#       the probability that it will live in an urban area two years
#       from now? A suburban area? A rural area?
#       (b) Suppose that at present, 40% of all families live in
#       an urban area, 35% live in a suburban area, and 25%
#       live in a rural area. Two years from now, what percent-
#       age of American families will live in an urban area?
#       (c) What problems might occur if this model were used
#       to predict the future population distribution of the United
#       States?

P = matrix(c(
    # U   # R   # S
    0.80, 0.05, 0.15, # U
    0.04, 0.90, 0.06, # R
    0.06, 0.04, 0.90  # S
), nrow=3, byrow=TRUE)
rownames(P) = colnames(P) = c('urban', 'rural', 'suburban')

P2 = P%*%P
a = P2[1, ]

b = c(0.4, 0.25, 0.35)%*%P2

#--------------------------------------------------
# PROBLEM 3
#--------------------------------------------------
# TODO: This one is just a las to copy over

#--------------------------------------------------
# TT01
#--------------------------------------------------
# TODO: Skipped to do the rest of the tuts

#----------------------------------------------------------------------------------------------------
# SECTION 17.4
#----------------------------------------------------------------------------------------------------
# TODO: This whole section is very theory heavy. Not much to write about in R

#----------------------------------------------------------------------------------------------------
# SECTION 17.4
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

P1 = matrix(c(
#   G     F     B
    0.85, 0.10, 0.05, # G
    0.00, 0.70, 0.30, # F
    1.00, 0.00, 0.00  # B
), nrow=3, byrow=TRUE)
rownames(P1) = rownames(P1) = c('G', 'F', 'B')

P2 = matrix(c(
#   G     F     B
    0.85, 0.10, 0.05, # G
    1.00, 0.00, 0.00, # F
    1.00, 0.00, 0.00  # B
), nrow=3, byrow=TRUE)
rownames(P2) = rownames(P2) = c('G', 'F', 'B')

cost_good = 6000
trade_in_fair = 2000
drive_cost_good = 1000
drive_cost_fair = 1500

c1 = c(
    drive_cost_good,
    drive_cost_fair,
    cost_good + drive_cost_good
)

get_PIj17 = function(P) {
    n = nrow(P)
    Pmod = P-diag(n)
    Pmod[,1] = 1
    return(c(1, rep(0, n-1))%*%solve(Pmod))
}

PIj1 = get_PIj17(P1)
cost1 = PIj1%*%c1

c2 = c(
    drive_cost_good,
    cost_good - trade_in_fair + drive_cost_good,
    cost_good + drive_cost_good
)
PIj2 = get_PIj17(P2)
cost2 = PIj2%*%c2

M11 = 1/PIj1[1]
M22 = 1/PIj1[2]
M33 = 1/PIj1[3]


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
# will sell tomorrow for $25. On the average, which stock will
# sell for a higher price? Find and interpret all mean first
# passage times.

P1 = matrix(c(
    0.80, 0.20,
    0.10, 0.90
), nrow=2, byrow=TRUE)
colnames(P1) = rownames(P1) = c('$10', '$20')

P2 = matrix(c(
    0.90, 0.10,
    0.15, 0.85
), nrow=2, byrow=TRUE)
colnames(P2) = rownames(P2) = c('$10', '$25')

PIj1 = get_PIj17(P1)
PIj2 = get_PIj17(P2)

# PIj1%*%c(10, 20)
# PIj2%*%c(10, 25)


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
#       (a) During what fraction of years will gray squirrels be
#       living in Pine Valley?
#       (b) During what fraction of years will black squirrels be
#       living in Pine Valley?

P = matrix(c(
    0.70, 0.20, 0.05, 0.05,
    0.20, 0.60, 0.10, 0.10,
    0.10, 0.10, 0.80, 0.00,
    0.05, 0.05, 0.10, 0.80
), nrow=4, byrow=TRUE)
rownames(P) = colnames(P) = c('G', 'B', 'GB', 'N')

PIj = get_PIj17(P)

# PIj[1] + PIj[3]

# PIj[2] + PIj[3]


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
# have an accident during the current year. During a given
# year, what is the average premium paid by a Payoff
# customer? (Hint: In case of difficulty, try a four-state Markov
# chain.)

P = matrix(c(
#   NN    NA    AA    AN
    0.97, 0.03, 0.00, 0.00, # NN
    0.00, 0.00, 0.10, 0.90, # NA
    0.00, 0.00, 0.10, 0.90, # AA
    0.97, 0.03, 0.00, 0.00  # AN
), nrow=4, byrow=TRUE)
rownames(P) = colnames(P) = c('NN', 'NA', 'AA', 'AN')

PIj = get_PIj17(P)
# PIj%*%c(100, 400, 400, 400)