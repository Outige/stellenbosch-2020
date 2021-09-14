source("/home/tieg/stellenbosch-2020/or322/a2/practice/testutils.R")


# ABSORBING STATE CHAPTER 21.3
# ----------------------------

# DEFAULT PROBLEM
#----------------
n = 520
seed = 987654321
x = seed
c = 100000
lambda = 1/13 * 60
t0 = 0
p = 0.3
vmin = 90/60
vmax = 300/60
ntests = 200

get_random_numbers3 = function(n, x, c, a=7^5, m=2^31-1) {
    random_numbers = c() # 0 <= random_numbers[i] < 1
    for (i in 1:n) {
        x = (a*x + c)%%m
        random_numbers[i] = x/m
    }
    return(random_numbers)
}

get_random_variables_exp3 = function(random_numbers, lambda) {
    return(
        qexp(random_numbers, lambda)
    )
}

get_random_variables_pois3 = function(random_numbers, lambda) {
    return(
        qpois(random_numbers, lambda)
    )
}


uniform_distribution = function(a, b, x) {
    return(a+(b-a)*x) # inverse transformation method
}

# a: min random variable
# b: max random variable
get_random_variables_uni3 = function(random_numbers, a, b) {
    random_variables = c()
    for (i in 1:length(random_numbers)) {
        random_variables[i] = uniform_distribution(a, b, random_numbers[i])
    }
    return(random_variables)
}

get_arrival_times3 = function(inter_arrival_times, t0) {
    arrival_times = c(t0)
    for (i in 2:length(inter_arrival_times)) {
        arrival_times[i] = sum(inter_arrival_times[1:i-1]) + t0
    }
    return(arrival_times)
}

get_bernoulli13 = function(n, seed, c, a, m, p) {
    random_numbers = get_random_numbers3(n, x, c, a, m)

    bernoulli = c()
    for (i in 1:n) {
        if (random_numbers[i] <= p) {
            bernoulli[i] = 1
        } else {
            bernoulli[i] = 0
        }
    }
    return(bernoulli)
}

get_binomial13 = function(n, seed, c, a, m, p, ntests) {
    x = seed
    upadte_seed = function(n, x, c, a, m) {
        for (i in 1:n) {
            x = (a*x + c)%%m
        }
        return(x)
    }

    trials = matrix(c(rep(0, ntests*n)), nrow=200)
    for (test in 1:ntests) {
        random_numbers = get_random_numbers3(n, x, c)
        x = upadte_seed(n, x, c, a, m)
        for (i in 1:n) {
            if (random_numbers[i] <= p) {
                random_numbers[i] = 1
            } else {
                random_numbers[i] = 0
            }
        }
        trials[test,] = random_numbers 
    }
    return(trials)
}

test_exp_arrival_times13 = function(random_numbers, random_variables, arrival_times, n, seed, c, a, m, lambda, t0) {
    x = seed
    random_numbers_ = get_random_numbers3(n, x, c, a , m)
    random_variables_ = get_random_variables_exp3(random_numbers_, lambda)
    arrival_times_ = get_arrival_times3(random_variables_, t0)
    
    random_numbers = matrix(random_numbers, nrow=1)
    random_numbers_ = matrix(random_numbers_, nrow=1)
    stopifnot(test_equal(random_numbers, random_numbers_))

    random_variables = matrix(random_variables, nrow=1)
    random_variables_ = matrix(random_variables_, nrow=1)
    stopifnot(test_equal(random_variables, random_variables_))

    arrival_times = matrix(arrival_times, nrow=1)
    arrival_times_ = matrix(arrival_times_, nrow=1)
    stopifnot(test_equal(arrival_times, arrival_times_))
}

test_pois13 = function(random_numbers, random_variables, n, seed, c, a, m, lambda) {
    x = seed
    random_numbers_ = get_random_numbers3(n, x, c, a , m)
    random_variables_ = get_random_variables_pois3(random_numbers_, lambda)
    
    random_numbers = matrix(random_numbers, nrow=1)
    random_numbers_ = matrix(random_numbers_, nrow=1)
    stopifnot(test_equal(random_numbers, random_numbers_))

    random_variables = matrix(random_variables, nrow=1)
    random_variables_ = matrix(random_variables_, nrow=1)
    stopifnot(test_equal(random_variables, random_variables_))
}

test_uni13 = function(random_numbers, random_variables, n, seed, c, a, m, lambda, vmin, vmax) {
    x = seed
    random_numbers_ = get_random_numbers3(n, x, c, a , m)
    random_variables_ = get_random_variables_uni3(random_numbers_, vmin, vmax)
    
    random_numbers = matrix(random_numbers, nrow=1)
    random_numbers_ = matrix(random_numbers_, nrow=1)
    stopifnot(test_equal(random_numbers, random_numbers_))

    random_variables = matrix(random_variables, nrow=1)
    random_variables_ = matrix(random_variables_, nrow=1)
    stopifnot(test_equal(random_variables, random_variables_))
}

test_bernoulli13 = function(bernoulli, n, seed, c, a, m, p) {
    bernoulli_ = get_bernoulli13(n, seed, c, a, m, p)
    bernoulli_ = matrix(bernoulli_, nrow=1)
    bernoulli = matrix(bernoulli, nrow=1)

    stopifnot(test_equal(bernoulli, bernoulli_))
}

test_binomial13 = function(binomial, n, seed, c, a, m, p, ntests) {
    binomial_ = get_binomial13(n, seed, c, a, m, p, ntests)
    stopifnot(test_equal(binomial, binomial_))
}