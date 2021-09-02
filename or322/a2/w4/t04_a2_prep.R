#----------------------------------------------------------------------------------------------------
# SECTION 20.2
#----------------------------------------------------------------------------------------------------

#--------------------------------------------------
# PROBLEM 2
#--------------------------------------------------
# The time between buses follows the mass function shown
# time            prob
# 30 minutes      1/4
# 60 minutes      1/4
# 120 minutes     1/2
#         (a) What is the average length of time one must wait
#         for a bus?

EA = c(30, 60, 120)%*%c(1/4, 1/4, 1/2)
E2A = c(30, 60, 120)^2%*%c(1/4, 1/4, 1/2)
var = E2A - EA^2
av_wait = 1/2*(EA + var/EA)
cat(sprintf("20.2: Problem 2: The average wait time is \u001b[36m%f\u001b[0m minutes or \u001b[36m%f\u001b[0m hours\u001b[31m(don't understand this formula)\u001b[0m\n\n", av_wait, av_wait/60))

#--------------------------------------------------
# PROBLEM 3
#--------------------------------------------------
# There are four sections of the third grade at Jefferson
# Elementary School. The number in each section is as
# follows: section 1, 20 students; section 2, 25 students;
# section 3, 35 students; section 4, 40 students.
#       (a) What is the average size of a third-grade section? Suppose
#       the board of education randomly selects a Jefferson third-grader.
#       (b) On the average, how many students will be in her class?

sections = c(20, 25, 35, 40)
cat(sprintf("20.2: Problem 3a: Average 3rd grade section is \u001b[36m%f\u001b[0m students\n", mean(sections)))

ans = sum((sections/sum(sections))*sections)
cat(sprintf("20.2: Problem 3b: On average there will be \u001b[36m%f\u001b[0m students in her class\n\n", ans))

#--------------------------------------------------
# PROBLEM 7
#--------------------------------------------------
# An average of 12 jobs per hour arrive at our departmental
# printer.
#       (a) Use two different computations (one involving the
#       Poisson and another the exponential random variable) to
#       determine the probability that no job will arrive during
#       the next 15 minutes.
#       (b) What is the probability that 5 or fewer jobs will ar-
#       rive during the next 30 minutes?

lambda = 12 # c/h
t=15/60
n=0
cat(sprintf("20.2: Problem 7a: The probability no job arives in the next 15 minutes is \u001b[36m%f\u001b[31m*\u001b[0m\n", ppois(n, lambda*t, lower.tail=TRUE)))

# pexp(t, lambda*t, lower.tail=TRUE) # TODO

n=5
t = 30/60
cat(sprintf("20.2: Problem 7b: The probability fewer than 5 jobs arives in the next 30 minutes is \u001b[36m%f\u001b[31m*\u001b[0m\n\n", ppois(n, lambda*t, lower.tail=TRUE)))


#----------------------------------------------------------------------------------------------------
# SECTION 20.3
#----------------------------------------------------------------------------------------------------

#--------------------------------------------------
# PROBLEM 2
#--------------------------------------------------
# My home uses two light bulbs. On average, a light bulb
# lasts for 22 days (exponentially distributed). When a light
# bulb burns out, it takes an average of 2 days (exponentially
# distributed) before I replace the bulb.
#       (a) Formulate a three-state birth–death model of this
#       situation.
#       (b) Determine the fraction of the time that both light
#       bulbs are working.
#       (c) Determine the fraction of the time that no light bulbs
#       are working.

#--------------------------------------------------
# PROBLEM 3
#--------------------------------------------------
cat(sprintf("\u001b[31m20.3 is a wash at the moment\u001b[0m\n\n", ppois(n, lambda*t, lower.tail=TRUE)))

quit()

#----------------------------------------------------------------------------------------------------
# SECTION 17.6
#----------------------------------------------------------------------------------------------------

#--------------------------------------------------
# PROBLEM 1
#--------------------------------------------------
#       (a) If a student enters State College as a freshman, how many years can he expect to spend as a student at State?
#       (b) What is the probability that a freshman graduates?

# GIVEN
P = matrix(c(
    0.10, 0.80, 0.00, 0.00, 0.10, 0.00,
    0.00, 0.10, 0.85, 0.00, 0.05, 0.00,
    0.00, 0.00, 0.15, 0.80, 0.05, 0.00,
    0.00, 0.00, 0.00, 0.10, 0.05, 0.85,
    0.00, 0.00, 0.00, 0.00, 1.00, 0.00,
    0.00, 0.00, 0.00, 0.00, 0.00, 1.00
), nrow=6, byrow=TRUE)
colnames(P) = rownames(P) = c("F.", "So.", "J.", "Sen.", "Q.", "G.")

Q = P[1:4, 1:4]
R = P[1:4, 5:6]
F = solve(diag(4) - Q)
A = F%*%R

cat(sprintf("17.6: Problem 1a: If a student enters as a freshman they will spend on average \u001b[36m%f\u001b[0m years at the college\n", sum(F[1,])))

cat(sprintf("17.6: Problem 1b: The probability a freshman graduates is \u001b[36m%f\u001b[0m\n\n", A[1, 2]))

#--------------------------------------------------
# PROBLEM 2
#--------------------------------------------------
# The Herald Tribble has obtained the following information about its subscribers:
# During the first year as subscribers, 20% of all subscribers cancel their
# subscriptions. Of those who have subscribed for one year, 10% cancel during
# the second year. Of those who have been subscribing for more than two years,
# 4% will cancel during any given year.
#         (a) On the average, how long
#         does a subscriber subscribe to the Herald Tribble?

P = matrix(c(
    0.00, 0.80, 0.00, 0.20,
    0.00, 0.00, 0.90, 0.10,
    0.00, 0.00, 0.96, 0.04,
    0.00, 0.00, 0.00, 1.00
), nrow=4, byrow=TRUE)
colnames(P) = rownames(P) = c("Y0", "Y1", "Y2+", "C")

Q = P[1:3, 1:3]
R = P[1:3, 4]
F = solve(diag(3)-Q)
A = F%*%R

cat(sprintf("17.6: Problem 2: On average a user subscibes to the herald for \u001b[36m%f\u001b[0m years\n\n", sum(F[1,])))


#--------------------------------------------------
# PROBLEM 5
#--------------------------------------------------
# Each week, the number of acceptable-quality units of a drug that
# are processed by a machine is observed: 100, 50–100,
# 1–50, 0 (indicating that the machine was broken during the week).
# Given last week’s observation, the probability distribution
# of next week’s observation is as follows.
# For example, if we observe a week in which more than 100 units are
# produced, then there is a .10 chance that during the next
# week 50–100 units are produced.
#       (a) Suppose last week the machine produced 200 units.
#       On average, how many weeks will elapse before the machine
#       breaks down?
#       (b) Suppose last week the machine produced 50 units.
#       On average, how many weeks will elapse before the machine breaks
#       down?

# GIVEN
P = matrix(c(
    0.80, 0.10, 0.05, 0.05,
    0.10, 0.60, 0.10, 0.20,
    0.10, 0.10, 0.50, 0.30,
    0.00, 0.00, 0.00, 1.00
), nrow=4, byrow=TRUE)
colnames(P) = rownames(P) = c(">100", "50-100", "1-50", "0")

Q = P[1:3, 1:3]
R = P[1:3, 4]
F = solve(diag(3)-Q)
A = F%*%R

cat(sprintf("17.6: Problem 5a: On average a machine that produces 200 units will take on average \u001b[36m%f\u001b[0m weeks to breakdown\n", sum(F[1,])))

cat(sprintf("17.6: Problem 5b: On average a machine that produces 50 units will take on average \u001b[36m%f\u001b[0m weeks to breakdown\n\n", sum(F[3,])))

#--------------------------------------------------
# PROBLEM 12
#--------------------------------------------------
cat(sprintf("17.6: Problem 12: \u001b[31mscuffed theory\u001b[0m\n\n"))


#--------------------------------------------------
# PROBLEM 16
#--------------------------------------------------
P = matrix(c(
    1.00, 0.00, 0.00, 0.00, 0.00, 0.00,
    0.00, 1.00, 0.00, 0.00, 0.00, 0.00,
    0.10, 0.30, 0.00, 0.25, 0.20, 0.15,
    0.05, 0.45, 0.00, 0.20, 0.20, 0.10,
    0.15, 0.10, 0.00, 0.15, 0.25, 0.35,
    0.20, 0.05, 0.00, 0.15, 0.30, 0.30
), nrow=6, byrow=TRUE)
colnames(P) = rownames(P) = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)")

Q = P[3:6, 3:6]
R = P[3:6, 1:2]
F = solve(diag(4)-Q)
A = F%*%R
cat(sprintf("17.6: Problem 16a: A new customer will take on average \u001b[36m%f\u001b[0m calls before a sale is made or lost\n", sum(F[1,])))
cat(sprintf("17.6: Problem 16b: Fraction of new customers who purchase the product is \u001b[36m%f\u001b[0m\n", A[1,1]))
cat(sprintf("17.6: Problem 16c: Fraction of low interest customers who purchase the product is \u001b[36m%f\u001b[0m\n", A[2,1]))

profit3 =  -sum(F[1,])*15 + A[1,]%*%c(190, 0)
profit4 =  -sum(F[2,])*15 + A[2,]%*%c(190, 0)
profit5 =  -sum(F[3,])*15 + A[3,]%*%c(190, 0)
profit6 =  -sum(F[4,])*15 + A[4,]%*%c(190, 0)
cat(sprintf("17.6: Problem 16d: Profit of each customer is: \u001b[36m{3: %f, 4: %f, 5: %f, 6: %f}\u001b[0m\n\n", profit3, profit4, profit5, profit6))


#----------------------------------------------------------------------------------------------------
# SECTION 17.7
#----------------------------------------------------------------------------------------------------

#--------------------------------------------------
# PROBLEM 1
#--------------------------------------------------
# Suuppose that each year state college admits 7000
# freshman, 500 sophmores, 500 juniors.
#       (a) What will the long run composition be?

# GIVEN
P = matrix(c(
    0.10, 0.80, 0.00, 0.00, 0.10, 0.00,
    0.00, 0.10, 0.85, 0.00, 0.05, 0.00,
    0.00, 0.00, 0.15, 0.80, 0.05, 0.00,
    0.00, 0.00, 0.00, 0.10, 0.05, 0.85,
    0.00, 0.00, 0.00, 0.00, 1.00, 0.00,
    0.00, 0.00, 0.00, 0.00, 0.00, 1.00
), nrow=6, byrow=TRUE)
colnames(P) = rownames(P) = c("F.", "So.", "J.", "Sen.", "Q.", "G.")
Q = P[1:4, 1:4]

bbar = c(7000, 500, 500, 0)
hbar = bbar %*% (diag(4)-Q)
# hbar

# bbar = hbar %*% solve(diag(4)-Q)
# rownames(bbar) = c('bbar')
# bbar
cat(sprintf("17.7: Problem 1: \u001b[31mhelp\u001b[0m\n\n"))

#--------------------------------------------------
# PROBLEM 2
#--------------------------------------------------

#--------------------------------------------------
# PROBLEM 4
#--------------------------------------------------

#--------------------------------------------------
# PROBLEM 7
#--------------------------------------------------