#----------------------------------------------------------------------------------------------------
# SECTION 21.3
#----------------------------------------------------------------------------------------------------
# NOW I'M JUST GOING TO FUNCTIONISE THE CODE
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

#--------------------------------------------------
# TUTORIAL
#--------------------------------------------------
#----------
# PROBLEM 1
#----------
# Generate a set of 520 arrival times in hours for trucks to a customs gate.
# Use a LCG with good parameters and a seed number of 987654321 and an increment of 100000.
# Interarrival times are exponential distributed with a mean time between arrivals of 13 minutes.
# Assume the first arrival occurs at time 0.

n = 520 # number of arrivals
seed = x = 987654321 # seed
c = 100000 # incriment
lambda = 1/13 * 60 # c/h
a = 7^5 # multiplier, standard good paramater
m = 2^31-1 # modulo, standard good paramater. prime and large
t0 = 0

random_numbers = get_random_numbers3(n, x, c)
inter_arrival_times = get_random_variables_exp3(random_numbers, lambda)
arrival_times = get_arrival_times3(inter_arrival_times, t0)

#----------
# PROBLEM 2
#----------
# Generate a set of 710 service times in minutes for trucks at tollgate.
# Use an LCG with good parameters and a seed number of 2000000 and an
# increment of 200000. Service times are uniform distributed with a minimum of
# 90 seconds and a maximum of 300 seconds.

n = 710
seed = x = 2000000
c = 200000
a = 7^5
m = 2^31-1
# mu_min = 1/90*60
# mu_max = 1/300*60
mu_min = 90/60 # TODO: how to choose (otherwise fine)
mu_max = 300/60

random_numbers = get_random_numbers3(n, x, c)
random_variables = get_random_variables_uni3(random_numbers, mu_min, mu_max)


#----------
# PROBLEM 3
#----------
# Rugby supporters going through a turnstile are either Bulls supporters or Stormer
# supporters. Simulate a stream of 450 supporters moving through the turnstile  at a game where
# 30% of supporters can be expected to be Bulls supporters, indicating a “B” or an “S” in the stream,
# based on team-loyalty. Use an LCG with good parameters and a seed number of 1000000 and an
# increment of 100001.

p = 0.3
n = 450
seed = x = 1000000
c = 100001
a = 7^5
m = 2^31-1

random_numbers = get_random_numbers3(n, x, c)

supporters = c()
for (i in 1:n) {
    if (random_numbers[i] <= p) {
        supporters[i] = 'B'
    } else {
        supporters[i] = 'S'
    }
}


#----------
# PROBLEM 4
#----------
# Each packet of Skittles contains 35 sweets of which 20% are red in colour.
# Simulate the counts of red sweets in a stream of 200 packets. Use a LCG with
# good parameters and a seed number of 2000001 and an increment of 101010.

tests = 200
n = 35
seed = x = 2000001
c = 101010
p = 0.2
a = 7^5
m = 2^31-1

upadte_seed = function(x, a, m, c, n) {
    for (i in 1:n) {
        x = (a*x + c)%%m
    }
    return(x)
}

trials = matrix(c(rep(0, tests*n)), nrow=200)
# trials
for (test in 1:tests) {
    random_numbers = get_random_numbers3(n, x, c)
    x = upadte_seed(x, a, m, c, n)
    for (i in 1:n) {
        if (random_numbers[i] <= p) {
            random_numbers[i] = 1
        } else {
            random_numbers[i] = 0
        }
    }
    trials[test,] = random_numbers 
}

# sum the number of sweets in a packet per trial
results = rep(0, tests)
for (i in 1:tests) {
    results[i] = sum(trials[i,])
}


#----------
# PROBLEM 5
#----------
# On average 17 barges arrive at a river port each day.
# Generate a stream of the number of barges arriving for
# one year of 365 days. Assume the number of arrivals per
# day follows the Poisson distribution. Use a LCG with good
# parameters and a seed number of 101010101 and an increment of 0.

lambda = 17 # barges/day
seed = x = 101010101
c = 0
a = 7^5
m = 2^31-1
n = 365

random_numbers = get_random_numbers3(n, seed, c)

random_variables = get_random_variables_pois3(random_numbers, lambda)

#--------------------------------------------------
# TT10
#--------------------------------------------------
# Create a set of arrival times in minutes, for 101 customers to an auto-bank
# where the first customer arrives at time 0 minutes.
# Interarrival times are exponential distributed, where the arrival rate
# is 20 customers per hour.
# Use a LCG with good parameters to create the required random numbers. Let the
# seed number for the LCG be 654321001 and let the increment be 1000007.
# Select the correct statements, regarding your calculations from the list of
# statements below. The selection of incorrect statements will lead to the deduction of marks.


n = 101
t0 = 0
lambda = 20 # c/h
x = 654321001
c = 1000007
random_numbers = get_random_numbers3(n, x, c)
inter_arrival_times = get_random_variables_exp3(random_numbers, lambda)
arrival_times = get_arrival_times3(inter_arrival_times, t0)

cat(sprintf("TT10: Problem 1: R_10 = \u001b[36m%f\u001b[0m(0.05048004)\n", random_numbers[10]))
cat(sprintf("TT10: Problem 2: \u001b[36m%s\u001b[0m\n", "The use of a prime number as a modulus ensures a cycle length in the generated values, of the magnitude of the modulus"))
cat(sprintf("TT10: Problem 3: \u001b[36m%s\u001b[0m\n", "The use of a non-prime number as a modulus ensures a cycle length in the generated values, which is shorter than the magnitude of the modulus"))


x = 654321001
x1 = ((7^5)*x+c)%%(2^31-1)
cat(sprintf("TT10: Problem 4: x_1 = \u001b[36m%f\u001b[0m(2057791174)\n", x1))


cat(sprintf("TT10: Problem 5:  The time elapsed between the arrivals of customers 10 and 11 is \u001b[36m%f\u001b[0m(0.15539617) minutes\n", inter_arrival_times[10]*60))
cat(sprintf("TT10: Problem 4: Customer number 13 arrives at time instant \u001b[36m%f(29.665020)\u001b[0m minutes.\n\n", arrival_times[13]*60))