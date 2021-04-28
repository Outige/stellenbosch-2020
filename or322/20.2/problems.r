#0----------------------------------------------
#0 PROBLEM 2
#0----------------------------------------------
# QUESTION

# The time between buses follows the mass function shown
# in Table 2. What is the average length of time one must wait
# for a bus?

# TABLE 2
# Time between busses | Probability
# ---------------------------------
# Thirty minutes      | 1/4
# One hour            | 1/4
# Two hours           | 1/2

# FORMULAS
# Average wait time = 1/2[E(A) + (V(A)/E(A))]
# E(A) = sum((ti)(pi))
# E(A^2) = sum((ti^2)(pi))
# V(A) = E(A^2) - E(A)^2

# WORKING OUT
ea = (30)*(1/4) + (60)*(1/4) + (120)*(1/2)
ea_sqaured = (30^2)*(1/4) + (60^2)*(1/4) + (120^2)*(1/2)
va = ea_sqaured - ea^2

average_wait_time = (1/2) * (ea + (va/ea))

# PRINT
# cat(sprintf("E(A) = %.2f\nEA(A^2) = %.2f\nV(A) = %.2f\nAverage wait time = %.2f\n", ea, ea_sqaured, va, average_wait_time))

#0----------------------------------------------
#0 PROBLEM 3
#0----------------------------------------------
# QUESTION

# There are four sections of the third grade at Jefferson
# Elementary School. The number in each section is as
# follows: section 1, 20 students; section 2, 25 students;
# section 3, 35 students; section 4, 40 students. What is the
# average size of a third-grade section? Suppose the board of
# education randomly selects a Jefferson third-grader. On the
# average, how many students will be in her class?

# SUB QUESTION:
# What is the average size of a third-grade section?
# = E(A)
ea = (20)*(1/4) + (25)*(1/4) + (34)*(1/4) + (40)*(1/4)
ea

# SUB QUESTION:
# Suppose the board of
# education randomly selects a Jefferson third-grader. On the
# average, how many students will be in her class?
# = 1/2[E(A) + (V(A)/E(A))]
ea = ea
ea_squared = (20^2)*(1/4) + (25^2)*(1/4) + (34^2)*(1/4) + (40^2)*(1/4)
va = ea_sqaured - ea^2
average_students = 1/2*(ea + (va/ea))

# ea
# ea_sqaured
# va
# average_students

#0----------------------------------------------
#0 PROBLEM 7b
#0----------------------------------------------
# FUNCTIONS
factorial <- function(n) {
    if (n == 0) {
        return(1)
    }
    factorial = 1
    for(i in 1:n) {
        factorial = factorial*i
    }
    return(factorial)
}

P7b <- function(lambda, t, n) {
    ans = 0

    # Sum profrom n = {0,...,n}
    for(i in 0:n) {
        # cat(sprintf("exp(-%.1f * %d)*(%.1f * %d)^(%d))/(factorial(%d)) = %.4f\n", lambda, t, lambda, t, i, i, (exp(-lambda*t)*(lambda*t)^(i))/(factorial(i))))
        ans = ans + (exp(-lambda*t)*(lambda*t)^(i))/(factorial(i))
    }
    return(ans)
}


lambda = 12/60
t = 30
n = 5

P7b(lambda, t, n)

# option 2. Using the built in ppois function.
# this does my function automatically. Its summing from n = {n,...,0}
ans = ppois(n, t*lambda, lower.tail = TRUE)
ans

# so if you want to calculate P(N = 5) you can do this
# the sum n = {5,..,0} - sum n = {4,..,0} = sum {5}
ans = ppois(4, t*lambda, lower.tail = TRUE) - ppois(4, t*lambda, lower.tail = TRUE)