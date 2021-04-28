# Example 20.2 1
factorial <- function(n) {
    factorial = 1
    for(i in 1:n) {
        factorial = factorial*i
    }
    return(factorial)
}

probability <- function(lmbda, n, t) {
    (exp(1)^(-lambda*t)*(lambda*t)^(n))/(factorial(n))
}

# 1.1
lambda = 30
n = 60
t = 2
probability(lambda, n, t)