#0----------------------------------------------
#0 example 2
#0----------------------------------------------
# Indiana Bell customer service representatives receive an average of 1,700 calls per hour.
# The time between calls follows an exponential distribution. A customer service represen-
# tative can handle an average of 30 calls per hour. The time required to handle a call is
# also exponentially distributed. Indiana Bell can put up to 25 people on hold. If 25 people
# are on hold, a call is lost to the system. Indiana Bell has 75 service representatives.

lambda_0 = 1700 #calls per hour
mu_1 = 30 #calls per hour

# lambda_0 to lambda_99
lambda_list=rep(lambda_0,100)
lambda_list
# lambda_list

# mu_1 to mu_100
mu_list=c(mu_1*c(1:75),rep(mu_1*75,25))
mu_list
# mu_list

# c_1 to c_100
c_list=c(lambda_list[1]/mu_list[1])
for(j in 2:100) c_list[j] = c_list[j-1] * (lambda_list[j]/mu_list[j])
c_list

# c_list

pi_0=1/(1+sum(c_list))

pi_list = c(c_list[1]*pi_0)
for (j in 2:100) pi_list[j] = c_list[j]*pi_0



#1--------------------
#1 Q1: What fraction of the time are all operators busy?
#1--------------------
# A: Sum(pi_j) 75 <= j <= 100
fraction_of_busy_operators = sum(pi_list[75:100])
cat(sprintf(" 1) The fraction of the time are all operators busy %.2f\n", fraction_of_busy_operators))

#1--------------------
#1 Q2: What fraction of all calls are lost to the system?
#1--------------------
# A: pi_j[n]
fraction_of_lost_calls = pi_list[100]
cat(sprintf(" 2) The fraction of all calls are lost to the system %.2f\n", fraction_of_lost_calls))

#1--------------------
#1 Q3: What is the average number of calls in the system at any moment?
#1--------------------
# A: Hmm. Needs explaining

# we matrix multiply a matrix the states, by the fraction of time we are at that state
states = 1:100
average_number_of_calls = sum(states%*%pi_list)
cat(sprintf(" 3) The average number of calls in the system at any moment is %.2f\n", average_number_of_calls))

#1--------------------
#1 Q4: For interest sake
#1--------------------

# we multiple states 1:75 by the time percentage, then 75* time percentage for the rest
cat("4a) The average number of operators that are busy at any moment is",sum(1:75%*%pi_list[1:75],75*pi_list[76:100]), "\n")

cat("4b) The average number of customers on hold at any moment is",sum(1:25%*%pi_list[76:100]), "\n")