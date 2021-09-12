# QUestion 1 ###################################################################

# Generate a list of random numbers
x = 654321001 
a = 7^5
c = 1000007
m = 2^31 - 1

n = 101 

Rnum_set = c()

for (i in 1:n){
  x = (a*x + c)%%m
  R = x/m
  Rnum_set[i] = R
}

# Transform random numbers to random variables from exp(lambda = 13/60) distrubution
lambda = 20/60 # cust/min
Rvar_set = qexp(Rnum_set, lambda)

# INTER arrival time
Rvar_set

# ARRIVAL times (cummulative)
Rvar_arrival = c()
Rvar_arrival[1] = 0
Rvar_arrival[2] = Rvar_set[1] + Rvar_arrival[1]

for (i in 3:n)Rvar_arrival[i] = Rvar_arrival[i-1]+Rvar_set[i-1]

Rvar_arrival

Rvar_arrival[13]
(a*x + c)%%m
Rnum_set[10]


Rvar_set[10]
Rvar_set[11]
Rvar_set
