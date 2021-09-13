source("/home/tieg/stellenbosch-2020/or322/a2/practice/testutils.R")


# ABSORBING STATE CHAPTER 21.3
# ----------------------------
n = 520
seed = 987654321
x = seed
c = 100000
lambda = 1/13 * 60
t0 = 0

get_exp_random_numbers3 = function(n, x, c, a, m) {
    random_numbers = c() # 0 <= random_numbers[i] < 1
    for (i in 1:n) {
        x = (a*x + c)%%m
        random_numbers[i] = x/m
    }
    return(random_numbers)
}

get_exp_inter_arrival_times3 = function(random_numbers, lambda) {
    return(qexp(random_numbers, lambda))
}

get_exp_arrival_times3 = function(inter_arrival_times, t0) {
    arrival_times = c(t0)
    for (i in 2:length(inter_arrival_times)) {
        arrival_times[i] = arrival_times[i-1] + inter_arrival_times[i]
    }
    return(arrival_times)
}

test_exp_gen13 = function(random_numbers, inter_arrival_times, arrival_times, n, seed, c, a, m, lambda, t0) {
    x = seed
    random_numbers_ = get_exp_random_numbers3(n, x, c, a , m)
    inter_arrival_times_ = get_exp_inter_arrival_times3(random_numbers_, lambda)
    arrival_times_ = get_exp_arrival_times3(inter_arrival_times_, t0)
    
    random_numbers = matrix(random_numbers, nrow=1)
    random_numbers_ = matrix(random_numbers_, nrow=1)
    stopifnot(test_equal(random_numbers, random_numbers_))

    inter_arrival_times = matrix(inter_arrival_times, nrow=1)
    inter_arrival_times_ = matrix(inter_arrival_times_, nrow=1)
    stopifnot(test_equal(inter_arrival_times, inter_arrival_times_))

    arrival_times = matrix(arrival_times, nrow=1)
    arrival_times_ = matrix(arrival_times_, nrow=1)
    stopifnot(test_equal(arrival_times, arrival_times_))
}