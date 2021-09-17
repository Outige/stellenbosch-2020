#----------------------------------------------------------------------------------------------------
# SECTION 21.5 AM & DM METHOD
#----------------------------------------------------------------------------------------------------

#----------------
# T03
#----------------
# Determine a series of 200 service times in minutes from a probability
# distribution following the function f(t) = +(sqrt(2/pi - (t-5)^2))
# with a minimum of 5-sqrt(2/pi) minutes and a maximum of 5 + sqrt(2/pi)
# minutes. Use the acceptance rejection method and a good LCG to determine
# the required random numbers.

n = 20000

m = 2^31-1
a = 7^5
c = 100100100
seed = x = 123456789

vmin = 5-sqrt(2/pi)
vmax = 5+sqrt(2/pi)
f = function(t) return(sqrt((2/pi)-(t-5)^2))
M = f(5) # plot this or use a loop

curve(f, vmin, vmax)

i = 1
random_variables = c()
while (i <= n) {
    x = (a*x+c)%%m
    r1 = x/m
    t = vmin+(vmax-vmin)*r1

    x = (a*x+c)%%m
    r2 = x/m

    if (r2 <= f(t)/M) {
        random_variables[i] = t
        i = i + 1
    }
}
hist(random_variables)


#----------------
# T04
#----------------
# Determine a series of 580 service times from a normal distribution 
# with a mean of 38 hours and a standard deviation of 4 hours. Use 
# the direct method and a good LCG to determine the required random 
# numbers.

mu = 38
sigma = 4

n = 580
a = 7^5
m = 2^31-1
c = 100100100
seed = x = 987654321

random_variables = c()

i = 1
while (i <= n) {
    x = (a*x+c)%%m
    r1 = x/m
    x = (a*x+c)%%m
    r2 = x/m

    z1 = sqrt(-2*log(r1))*sin(2*pi*r2)

    random_variables[i] = mu + sigma*z1

    i = i + 1
}

hist(random_variables)

#---------------
# TT11 PROBLEM 1
#---------------
# Say the direct method is used to generate service times in 
# seconds, in a simulation and it is known that the service times 
# follow a normal distribution with a mean of 120 and a standard 
# deviation of 30. Say that during an iteration of the process, you 
# have generated the random numbers r_i1=0.34 and r_i2=0.72, what is 
# the value of the normal distributed variable generated?

mu = 120
sigma = 30
r1 = 0.34
r2 = 0.72
z1 = sqrt(-2*log(r1))*sin(2*pi*r2)
x1 = mu + sigma*z1
x1


#---------------
# TT11 PROBLEM 2
#---------------
# Say the direct method is used to generate service times in 
# seconds, in a simulation and it is known that the service times 
# follow a normal distribution with a mean of 120 and a standard 
# deviation of 30. Say that during an iteration i of the process, 
# you have generated the random numbers r_i1=0.34 and r_i2=0.72, 
# what is the value of the standard normal variable calculated?

z1