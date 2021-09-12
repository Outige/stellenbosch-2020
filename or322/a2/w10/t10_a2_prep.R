#----------------------------------------------------------------------------------------------------
# SECTION 21.3
#----------------------------------------------------------------------------------------------------

#--------------------------------------------------
# TUTORIAL
#--------------------------------------------------
# Generate a set of 520 arrival times in hours for trucks to a customs gate.
# Use a LCG with good parameters and a seed number of 987654321 and an increment of 100000.
# Interarrival times are exponential distributed with a mean time between arrivals of 13 minutes.
# Assume the first arrival occurs at time 0.

# STEP 1) generate a list of random numbers
number_arrivals = 520 # number of arrivals
x = 987654321 # seed
c = 100000 # incriment
lambda = 1/13 * 60 # c/h
a = 7^5 # multiplier, standard good paramater
m = 2^31-1 # modulo, standard good paramater. prime and large
arrival0 = 0

random_numbers = c() # 0 <= random_numbers[i] < 1
for (i in 1:number_arrivals) {
    x = (a*x + c)%%m
    random_numbers[i] = x/m
}

# NOTE: all of the variables will be given. a = 7^5 & m = 2^31-1 are just standard good paramaters
# NOTE: this step is very easy. all of the data will be given(just remeber your good paramaters).
#       also the formulas for how to use the inputs are all given


# STEP 2) make the random data fit the problem
inter_arrival_times = qexp(random_numbers, lambda)

# NOTE: just remember the qexp function and this step is also vv easy


# STEP 3) map inter arrival times to arrival times
arrival_times = c(arrival0)
for (i in 2:number_arrivals) {
    arrival_times[i] = arrival_times[i-1] + inter_arrival_times[i] # TODO: just check this
}


# NOW I'M JUST GOING TO FUNCTIONISE THE CODE
get_random_numbers3 = function(n, x, c, a=7^5, m=2^31-1) {
    random_numbers = c() # 0 <= random_numbers[i] < 1
    for (i in 1:n) {
        x = (a*x + c)%%m
        random_numbers[i] = x/m
    }
    return(random_numbers)
}

get_inter_arrival_times3 = function(random_numbers, lambda) {
    return(qexp(random_numbers, lambda))
}

get_arrival_times3 = function(inter_arrival_times, t0) {
    arrival_times = c(t0)
    for (i in 2:length(inter_arrival_times)) {
        arrival_times[i] = arrival_times[i-1] + inter_arrival_times[i]
    }
    return(arrival_times)
}

x = 987654321
random_numbers = get_random_numbers3(number_arrivals, x, c)
inter_arrival_times = get_inter_arrival_times3(random_numbers, lambda)
arrival_times = get_arrival_times3(inter_arrival_times, arrival0)

#--------------------------------------------------
# TUTORIAL TEST
#--------------------------------------------------
# Create a set of arrival times in minutes, for 101 customers to an auto-bank
# where the first customer arrives at time 0 minutes.
# Interarrival times are exponential distributed, where the arrival rate is
# 20 customers per hour.
# Use a LCG with good parameters to create the required random numbers.
# Let the seed number for the LCG be 654321001 and let the increment be 1000007.

n = 101
t0 = 0
lambda = 20 # c/h
x = 654321001
c = 1000007
random_numbers = get_random_numbers3(number_arrivals, x, c)
inter_arrival_times = get_inter_arrival_times3(random_numbers, lambda)
arrival_times = get_arrival_times3(inter_arrival_times, arrival0)

cat(sprintf("TT10: Problem 1: R_10 = \u001b[36m%f\u001b[0m(0.05048004)\n", random_numbers[10]))
cat(sprintf("TT10: Problem 2: \u001b[36m%s\u001b[0m\n", "The use of a prime number as a modulus ensures a cycle length in the generated values, of the magnitude of the modulus"))
cat(sprintf("TT10: Problem 3: \u001b[36m%s\u001b[0m\n", "The use of a non-prime number as a modulus ensures a cycle length in the generated values, which is shorter than the magnitude of the modulus"))


x = 654321001
x1 = (a*x+c)%%(2^31-1)
cat(sprintf("TT10: Problem 4: x_1 = \u001b[36m%f\u001b[0m(2057791174)\n", x1))


cat(sprintf("TT10: Problem 5:  The time elapsed between the arrivals of customers 10 and 11 is \u001b[36m%f\u001b[0m(0.15539617) minutes\n", inter_arrival_times[10]*60))

cat(sprintf("TT10: Problem 4: Customer number 13 arrives at time instant \u001b[36m%f\u001b[31m(29.665020)\u001b[0m minutes.\n\n", arrival_times[13]*60))