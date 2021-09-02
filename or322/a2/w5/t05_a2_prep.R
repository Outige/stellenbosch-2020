#----------------------------------------------------------------------------------------------------
# SECTION 20.4
#----------------------------------------------------------------------------------------------------

#--------------------------------------------------
# PROBLEM 2
#--------------------------------------------------
# The Decision Sciences Department is trying to
# determine whether to rent a slow or a fast copier. The
# department believes that an employee’s time is worth $15
# per hour. The slow copier rents for $4 per hour and it takes
# an employee an average of 10 minutes to complete copying
# (exponentially distributed). The fast copier rents for $15 per
# hour and it takes an employee an average of 6 minutes to
# complete copying. An average of 4 employees per hour need
# to use the copying machine (interarrival times are
# exponential). Which machine should the department rent?

lambda = 4 # c/h

s = 1
costemployee = 15 # $/h
mufast = 1/6*60 # c/h
costfast = 15 # $/h
muslow = 1/10*60 # c/h
costslow = 4 # $/h

rho6 = function(lambda, mu, s) {
    return(lambda/(mu*s))
}

PI06 = function(lambda, mu, s) {
    D = 0
    for (j in 0:(s-1)) {
        D = D + (s*rho6(lambda, mu, s))^j/factorial(j)
    }
    D = D + (s*rho6(lambda, mu, s))^s/(factorial(s)*(1-rho6(lambda, mu, s)))
    return(1/D)
}

PIj6 = function(lambda, mu, s, j) {
    if (j < s) {
        return(
            ((s*rho6(lambda, mu, s))^j*PI06(lambda, mu, s))/factorial(j)
        )
    } else {
        return(
            ((s*rho6(lambda, mu, s))^j*PI06(lambda, mu, s))/(factorial(s)*s^(j-s))
        )
    }
}

Pj6 = function(lambda, mu, s) {
    return(
        PI06(lambda, mu, s)*((s*rho6(lambda, mu, s))^s)/(factorial(s)*(1-rho6(lambda, mu, s)))
    )
}

Lq6 = function(lambda, mu, s) {
    return(
        ( Pj6(lambda, mu, s)*rho6(lambda, mu, s) ) / ( 1 - rho6(lambda, mu, s) )
    )
}

Wq6 = function(lambda, mu, s) {
    return(
        Lq6(lambda, mu, s)/lambda
    )
}

Ls6 = function(lambda, mu) {
    return(
        lambda/mu
    )
}

Ws6 = function(lambda, mu) {
    return(
        Ls6(lambda, mu)/lambda
    )
}


L6 = function(lambda, mu, s) {
    return(
        Lq6(lambda, mu, s) + Ls6(lambda, mu)
    )
}

W6 = function(lambda, mu, s) {
    return(
        L6(lambda, mu, s)/lambda
    )
}



cost = function(lambda, mu, s, printer_cost, employee_cost) {
    return(
        L6(lambda, mu, s)*employee_cost + printer_cost
    )
}

cfast = cost(lambda, mufast, s, costfast, costemployee)
cslow = cost(lambda, muslow, s, costslow, costemployee)
cat(sprintf("20.4: Problem 2: Cost of the fast system is \u001b[36m%f$/h\u001b[0m. The cost of the slow system is \u001b[36m%f$/h\u001b[0m\n\n", cfast, cslow))

#--------------------------------------------------
# PROBLEM 4
#--------------------------------------------------
# A fast-food restaurant has one drive-through window.
# An average of 40 customers per hour arrive at the window.
# It takes an average of 1 minute to serve a customer. Assume
# that interarrival and service times are exponential.
#       (a) On the average, how many customers are waiting in
#       line?
#       (b) On the average, how long does a customer spend at
#       the restaurant (from time of arrival to time service is
#       completed)?
#       (c) What fraction of the time are more than 3 cars waiting
#       for service (this includes the car (if any) at the window)?

lambda = 40 # c/h
s = 1
mu = 1*60 # c/h



cat(sprintf("20.4: Problem 4a: On average there are \u001b[36m%f\u001b[0m customers waiting in line\n", Lq6(lambda, mu, s)))
cat(sprintf("20.4: Problem 4b: On average a customer will spend \u001b[36m%f\u001b[0m hours at the restaurant\n", W6(lambda, mu, s)))

sigmaPIj1to3 = 0
for (j in 0:3) {
    sigmaPIj1to3 = sigmaPIj1to3 + PIj6(lambda, mu, s, j)
}
cat(sprintf("20.4: Problem 4c: The fraction of time more than 3 cars in the system is \u001b[36m%f\u001b[0m\n\n", 1-sigmaPIj1to3))



#--------------------------------------------------
# PROBLEM 6
#--------------------------------------------------
# Our local maternity ward delivers 1,500 babies per year.
# On the average, 5 beds in the maternity ward are filled. How
# long does the average mother stay in the maternity ward?

lambda = 1500 # c/y
L = 5
W = L/lambda*365
cat(sprintf("20.4: Problem 6: The average wait in the maternity ward is \u001b[36m%f\u001b[0m days\n\n", W))


#--------------------------------------------------
# PROBLEM 10
#--------------------------------------------------
# Consider an airport where taxis and customers arrive
# (exponential interarrival times) with respective rates of 1
# and 2 per minute. No matter how many other taxis are
# present, a taxi will wait. If an arriving customer does not
# find a taxi, the customer immediately leaves.
#       (a) Model this system as a birth–death process (Hint:
#       Determine what the state of the system is at any given
#       time and draw a rate diagram.)
#       (b) Find the average number of taxis that are waiting for
#       a customer.
#       (c) Suppose all customers who use a taxi pay a $2 fare.
#       During a typical hour, how much revenue will the taxis
#       receive?

lambda = 2 # c/m, M
mu = 1 # c/m, M
s = 1

Lq6(lambda, mu, s)

Ls6(lambda, mu)*2*60

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