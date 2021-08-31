#0----------------------------------------
#0 CHI-SQUARED FUNCTIONS
#0----------------------------------------
chi_exp = function(data, lambdahat=-1) {
  #Find the number intervals to use in the test by Sturge's rule.
  k=ceiling(log(length(data),2))+1 # k = ⌈log2(N)⌉+1

  # Estimate lambda hat because it isn't given
  r = 0 # no unknowns
  if (lambdahat == -1) {
    lambdahat=1/mean(data) # TODO: lambdahat vs lambda
    r = 1 # inc unknown count (lambda unknown)
  }

  #Set bounds for the counting intervals (formula from textbook)
  upper_bounds=c()
  for(i in 1:k-1) { #? TODO:  for(i in 1:(k-1)) {??
      upper_bounds[i]=(-1/lambdahat)*log(1-(i/k)) # ub[i] = -1/λ*log10(1-i/k)
  }
  upper_bounds[k]=max(data)+1

  lower_bounds=c(0) # TODO: lower_bounds=c( min(data)-1 )
  for(i in 2:k) {
      lower_bounds[i]=upper_bounds[i-1] # current lower = previous upper
  }

  #Prepare a structure for the display of the results
  myDF=data.frame(lower_bounds,upper_bounds)

  o_i=rep(0,k)

  # Do the counting
  for(j in 1:length(data)){
    for(i in 1:k)if(data[j]>=lower_bounds[i]&&data[j]<=upper_bounds[i])o_i[i]=o_i[i]+1
  }

  #View the result of the counting
  o_i

  #Add the counting result to the display structure
  myDF=cbind(myDF,o_i)


  #Calculate the expected counts in each interval 
  #(expected counts is the same over all intervals; number of data points/number of intervals)
  e_i=rep(length(data)/k,k)  


  # Add the expeceted counts to the dispaly structure
  myDF=cbind(myDF,e_i)



  #Calculate the chis squeared statistic for the data. (Formula)
  chi=sum((o_i-e_i)^2/e_i)


  #calculate the critical chisquared value to compare against
  alpha=0.05

  df=k-r-1

  critical_value=qchisq(alpha,df,lower.tail = FALSE)


  #H_0: the interarrival times are exponentially distributed with lambdahat=...
  #Pronounce on the set hypothesis based on the result of the comparisons
  if (chi<critical_value) {
      cat(sprintf("chi: %f < cv: %f. Do not reject H_0\n", chi, critical_value))
  } else {
      cat(sprintf("chi: %f >= cv: %f. Reject H_0\n", chi, critical_value))
  }
}

#1-----------------------------------------------------
#1 Q1
#1-----------------------------------------------------
# Question: Check if these(Q1data) inter-arrival times can be modelled by the exponential distribution.
cat("Q1:\n")
Q1data=scan("Tut9_data/Tut9_Q1.txt") # read in Q1 data
chi_exp(Q1data) # lambdahat not supplied

# NOTE: degree_of_freedom = k - r - 1; where r is the number of unknowns

# Q1:
# Read 250 items
# chi: 1.640000 < cv: 14.067140. Do not reject H_0
# Dprimen: 0.568165 <= critical_value: 1.094000 => FALSE

# Q2:
# Read 240 items
# chi: 37.725000 >= cv: 15.507313. Reject H_0
# Dprimen: 2.699582 > critical_value: 1.358000 => TRUE

# Q3:
# Read 320 items
# chi: 176.937500 >= cv: 16.918978. Reject H_0
# Dprimen: 4.241036 > critical_value: 1.358000 => TRUE

# Q4:
# Read 350 items
# chi: 105.485714 >= cv: 14.067140. Reject H_0
# Dprimen: 2.403560 > critical_value: 0.895000 => TRUE