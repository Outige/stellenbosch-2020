# KS NOTES:
# ---------
# NOTE: lambdahat is, "paramater of the exponential distribution"
# NOTE: row referes to the KS table row
# NOTE: if not specified, confidence = 0.95 # TODO: is this true?

# KOLMOGOROV-SMIRNOV FUNCTIONS
# ----------------------------

# EXPONENTIAL
ks_exp = function(data, confidence=0.95, lambdahat=-1) {
  # 1) sort data
  data_sorted=sort(data)
  n=length(data_sorted)
  
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

  return(c(DnAdjusted, critical_value))
}

ks_norm = function(data, confidence, mu=-1, sigma=-1) {
  data_sorted=sort(data)
  # data_sorted
  n=length(data_sorted)
  row = 0
  if (mu == -1) {
    mu=sum(data)/length(data)
    row = 1
  }
  if (sigma == -1) {
    sigma=sqrt(var(data))
    row = 1
  }
  #The following set of statements are the required steps 
  #of the KS-test
  #Initialize the vectors to contain the calculated differences
  DnMinusSet=c()
  DnPlusSet=c()
  #calculate the absolute value of the differences
  for(i in 1:n){
    DnPlusSet[i]=abs((i/n)-pnorm(data_sorted[i],mu,sigma,lower.tail = TRUE,log.p=FALSE))
    DnMinusSet[i]=abs(pnorm(data_sorted[i],mu,sigma,lower.tail = TRUE,log.p=FALSE)-((i-1)/n))
  }
  # DnPlusSet
  # DnMinusSet
  DnPlus=max(DnPlusSet)
  DnMinus=max(DnMinusSet)
  Dn=max(DnMinus,DnPlus)
  # Dn
  #Calulate the adjusted D value relevant to the tested hypothesis.
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
  # DnAdjusted
  #Compare the adjusted D value with the 
  #relevant critical value from the table 
  #on slide 9. (The hypothesis determines the
  #row from which the formula for the adjusted
  #D value AND the critical value are read )
  if (DnAdjusted>critical_value) {
    cat(sprintf("DnAdjusted: %f > critical_value: %f => TRUE\n", DnAdjusted, critical_value, DnAdjusted>critical_value))
  } else {
    cat(sprintf("DnAdjusted: %f <= critical_value: %f => FALSE\n", DnAdjusted, critical_value, DnAdjusted>critical_value))
  }
}

ks_test = function(f, data) {
  DnAdjusted = f(data)[1]
  critical_value = f(data)[2]
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

# PROBLEM 1
# ---------
# Question: Check if these(Q1data) inter-arrival times can be modelled by the exponential distribution.
Q1Data=scan("Tut9_data/Tut9_Q1.txt")
Q2Data=scan("Tut9_data/Tut9_Q2.txt")
Q3Data=scan("Tut9_data/Tut9_Q3.txt")
Q4Data=scan("Tut9_data/Tut9_Q4.txt")
Q5Data=scan("Tut9_data/Tut9_Q5.txt")
# chi_exp(Q1data)
# ks_output = 

cat(sprintf("\nT09 PROBLEM 1a: The inter-arrival times can be modelled by the KS exponential distribution \u001b[36m?\u001b[0m\n"))
cat(sprintf("T09 PROBLEM 1b: The inter-arrival times can be modelled by the KS exponential distribution \u001b[36m%s\u001b[0m\n\n", ks_test(ks_exp, Q1Data)))


# cat("\nQ2:\n")
# Q2data=scan("Tut9_data/Tut9_Q2.txt")
# # chi_exp(Q2data, lambdahat=1/3)
# ks_exp(data=Q2data, confidence=0.95, lambdahat=1/3)


# cat("\nQ3:\n")
# Q3data=scan("Tut9_data/Tut9_Q3.txt")
# # chi_norm(Q3data, mu=480, sigma=40)
# ks_norm(data=Q3data, confidence=0.95, mu=480, sigma=40)


# cat("\nQ4:\n")
# Q4data=scan("Tut9_data/Tut9_Q4.txt")
# # chi_norm(Q4data)
# ks_norm(data=Q4data, confidence=0.95)