#0----------------------------------------------
#0 Q2
#0----------------------------------------------
# My home uses two light bulbs. On average, a light bulb
# lasts for 22 days (exponentially distributed). When a light
# bulb burns out, it takes an average of 2 days (exponentially
# distributed) before I replace the bulb.

# [l0, l1]
lambda_list = c(1, 1/2)

# [mu1, mu2]
mu_list = c(1/22, 2/22)

c_list = c()
c_list[1] = lambda_list[1]/mu_list[1]
c_list[2] = c_list[1]*(lambda_list[2]/mu_list[2])

pi_0 = 1/(1+sum(c_list))

pi_list = c(pi_0, c_list*pi_0)

#1---------------------
#1 Q2b
#1---------------------
# Determine the fraction of the time that both light
# bulbs are working.

cat(sprintf("The fraction of the time that both light bulbs are working %.4f\n", pi_list[3]))

#1---------------------
#1 Q2c
#1---------------------
# Determine the fraction of the time that no light bulbs
# are working.

cat(sprintf("The fraction of the time no light bulbs are working %.6f\n", pi_list[1]))

#0----------------------------------------------
#0 Q3
#0----------------------------------------------