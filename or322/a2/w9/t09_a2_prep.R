# KS NOTES:
# ---------
# NOTE: lambdahat is, "paramater of the exponential distribution"
# NOTE: row referes to the KS table row
# NOTE: if not specified, confidence = 0.95 # TODO: is this true?

# KOLMOGOROV-SMIRNOV FUNCTIONS
# ----------------------------

# EXPONENTIAL
ks_exp = function(data, confidence=0.95, lambdahat=-1) {
  # 1) sort data and get n
  data_sorted = sort(data)
  n = length(data_sorted)
  
  # 2) determine lmabdahat (if required)
  row = 0
  if (lambdahat == -1) {
    lambdahat=1/mean(data) # TODO: n/mean(data)?
    row = 2
  }

  DnMinusSet=c() # init vectors to store the absolute differences
  DnPlusSet=c()
  for(i in 1:n){
    F = pexp(data_sorted[i],lambdahat,lower.tail = TRUE,log.p=FALSE)
    DnPlusSet[i]=abs((i/n)-F)
    DnMinusSet[i]=abs(F-((i-1)/n))
  }

  Dn = max(DnPlusSet, DnMinusSet)
  
  # Calulate the adjusted D value relevant to the tested hypothesis.
  DnAdjusted=-1
  critical_value=-1
  if (row == 0) {
    DnAdjusted=(sqrt(n)+0.12+(0.11/sqrt(n)))*Dn
    if (confidence == 0.9) critical_value = 1.224
    else if (confidence == 0.95) critical_value = 1.358
    else if (confidence == 0.975) critical_value = 1.480
    else if (confidence == 0.99) critical_value = 1.628
  } else if (row == 2) {
    DnAdjusted=(Dn-(0.2/n))*(sqrt(n)+0.26+(0.5/sqrt(n)))
    if (confidence == 0.9) critical_value = 0.990
    else if (confidence == 0.95) critical_value =  1.094
    else if (confidence == 0.975) critical_value = 1.190
    else if (confidence == 0.99) critical_value = 1.308
  }

  out = matrix(c(DnAdjusted, critical_value), nrow=1)
  colnames(out) = c('Dn Adjusted', 'Critical Value')
  return(out)
}

ks_norm = function(data, confidence=0.95, mu=-1, sigma=-1) {
  # 1) sort data and get n
  data_sorted=sort(data)
  n=length(data_sorted)
  row = 0 # always start at row 0


  # 2) (optional) mean must be aproximated
  if (mu == -1) {
    mu=sum(data)/length(data)
    row = 1
  }


  # 3) (optional) standard deviation(sigma squared) must be aproximated
  if (sigma == -1) {
    sigma=sqrt(var(data))
    row = 1
  }


  # 4) determine plus and minus sets (identical to ks_exp)
  DnMinusSet=c()
  DnPlusSet=c()
  for(i in 1:n){
    F = pnorm(data_sorted[i],mu,sigma,lower.tail = TRUE,log.p=FALSE)
    DnPlusSet[i]=abs((i/n)-F)
    DnMinusSet[i]=abs(F-((i-1)/n))
  }
  Dn=max(DnMinusSet, DnPlusSet)


  # 5) determin DnAdjusted and critical_value from ks table
  DnAdjusted=0
  critical_value=0
  if (row == 0) {
    if (confidence == 0.9) critical_value = 1.224
    else if (confidence == 0.95) critical_value = 1.358
    else if (confidence == 0.975) critical_value = 1.480
    else if (confidence == 0.99) critical_value = 1.628
    else {
          cat(sprintf("Throw error: confidence value(%f) is invalid\n", confidence))
          quit()
    }
    DnAdjusted=(sqrt(n)+0.12+(0.11/sqrt(n)))*Dn
  } else if (row == 1) {
    if (confidence == 0.9) critical_value = 0.819
    else if (confidence == 0.95) critical_value =  0.895
    else if (confidence == 0.975) critical_value = 0.955
    else if (confidence == 0.99) critical_value = 1.035
    else {
          cat(sprintf("Throw error: confidence value(%f) is invalid\n", confidence))
          quit()
    }
    DnAdjusted=(sqrt(n)-0.01+(0.85/sqrt(n)))*Dn
  } else {
    cat(sprintf("Throw error: Row value(%d) is invalid\n", row))
    quit()
  }

  result = matrix(c(DnAdjusted, critical_value), nrow=1)
  colnames(result) = c('Dn Adjusted', 'Critical Value')
  return(result)
}

ks_test = function(input) {
  DnAdjusted = input[1]
  critical_value = input[2]
  if (DnAdjusted > critical_value) return(TRUE)
  return(FALSE)
}

 #else if (row == 1) {
    # if (confidence == 0.9) critical_value = 1.224
    # else if (confidence == 0.95) critical_value = 1.358
    # else if (confidence == 0.975) critical_value = 1.480
    # else if (confidence == 0.99) critical_value = 1.628
    # else {
    #       cat(sprintf("Throw error: confidence value(%f) is invalid\n", confidence))
    #       quit()
    # }
    # DnAdjusted=(Dn-(0.2/n))*(sqrt(n)+0.26+(0.5/sqrt(n)))

Q1Data=scan("Tut9_data/Tut9_Q1.txt")
Q2Data=scan("Tut9_data/Tut9_Q2.txt")
Q3Data=scan("Tut9_data/Tut9_Q3.txt")
Q4Data=scan("Tut9_data/Tut9_Q4.txt")
Q5Data=scan("Tut9_data/Tut9_Q5.txt")

# PROBLEM 1
# ---------
# Check if these inter-arrival times can be modelled by the exponential distribution.

cat(sprintf("\nT09 PROBLEM 1a: TODO: CS test \u001b[36m?\u001b[0m\n"))
cat(sprintf("T09 PROBLEM 1b: The inter-arrival times can be modelled by the KS exponential distribution \u001b[36m%s\u001b[0m\n\n", ks_test( ks_exp(Q1Data) )))



# PROBLEM 2
# ---------
# Check if these inter-arrival times can be modelled by the exponential distribution with lambda=1/3.
cat(sprintf("\nT09 PROBLEM 2a: TODO: CS test \u001b[36m?\u001b[0m\n"))
cat(sprintf("T09 PROBLEM 2b: The inter-arrival times can be modelled by the KS exponential distribution with lambda=1/3 \u001b[36m%s\u001b[0m\n\n", ks_test( ks_exp(Q2Data, lambdahat=1/3) )))



# PROBLEM 3
# ---------
# Check if these service times can be modelled by the normal distribution with a mean of 480 and a standard deviation of 40.
cat(sprintf("\nT09 PROBLEM 3a: TODO: CS test \u001b[36m?\u001b[0m\n"))
cat(sprintf("T09 PROBLEM 3b: The inter-arrival times can be modelled by the KS normal distribution with mean of 480 and a standard deviation of 40 \u001b[36m%s\u001b[0m\n\n", ks_test( ks_norm(Q1Data, mu=480, sigma=40) )))



# PROBLEM 4
# ---------
# Check if these service times can be modelled by the normal distribution.
cat(sprintf("\nT09 PROBLEM 4a: TODO: CS test \u001b[36m?\u001b[0m\n"))
cat(sprintf("T09 PROBLEM 4b: The inter-arrival times can be modelled by the KS normal distribution \u001b[36m%s\u001b[0m\n\n", ks_test( ks_norm(Q1Data) )))



# PROBLEM 5
# ---------
# Check if these service times can be modelled by the uniform distribution.
cat(sprintf("\nT09 PROBLEM 4a: TODO: CS test \u001b[36m?\u001b[0m\n"))
cat(sprintf("T09 PROBLEM 4b: \u001b[31m%s\u001b[0m\n\n", "Do we need to do this one?"))