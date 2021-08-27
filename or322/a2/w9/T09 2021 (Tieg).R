#0----------------------------------------
#0 CHI-SQUARED FUNCTIONS
#0----------------------------------------
chi_exp = function(data, lambdahat=-1) {
  #Find the number intervals to use in the test by Sturge's rule.
  k=ceiling(log(length(data),2))+1

  # Estimate lambda hat because it isn't given
  r = 0
  if (lambdahat == -1) {
    lambdahat=1/mean(data)
    r = 1
  }

  #Set bounds for the counting intervals (formula from textbook)
  ub_i=c()
  for(i in 1:k-1)ub_i[i]=(-1/lambdahat)*log(1-(i/k))
  ub_i[k]=max(data)+1

  lb_i=c(0)
  for(i in 2:k)lb_i[i]=ub_i[i-1]

  #Prepare a structure for the display of the results
  myDF=data.frame(lb_i,ub_i)

  o_i=rep(0,k)

  # Do the counting
  for(j in 1:length(data)){
    for(i in 1:k)if(data[j]>=lb_i[i]&&data[j]<=ub_i[i])o_i[i]=o_i[i]+1
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

chi_norm = function(data, mu=-1, sigma=-1) {
  #Find the number intervals to use in the test by Sturge's rule.
  k=ceiling(log(length(data),2))+1

  # Estimate lambda hat because it isn't given
  r = 0
  if (mu == -1) {
    mu=sum(data)/length(data)
    r = r + 1
  }
  if (sigma == -1) {
    sigma=sqrt(var(data))
    r = r + 1
  }


  #Set bounds for the counting intervals (formula from textbook)
  ub_i=c()
  for(i in 1:k-1)ub_i[i]=qnorm(i/k,mean=mu,sd=sigma,lower.tail=TRUE,log.p = FALSE)
  ub_i[k]=max(data)+1
  ub_i

  lb_i=c(0)
  for(i in 2:k)lb_i[i]=ub_i[i-1]

  #Prepare a structure for the display of the results
  myDF=data.frame(lb_i,ub_i)

  o_i=rep(0,k)

  # Do the counting
  for(j in 1:length(data)){
    for(i in 1:k)if(data[j]>=lb_i[i]&&data[j]<=ub_i[i])o_i[i]=o_i[i]+1
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


#0----------------------------------------
#0 KOLMOGOROV-SMIRNOV FUNCTIONS
#0----------------------------------------
ks_exp = function(data, confidence, lambdahat=-1) {
  data_sorted=sort(data)
  # data_sorted
  n=length(data_sorted)
  row = 0
  if (lambdahat == -1) {
    lambdahat=1/mean(data)
    row = 2
  }
  #The following set of statements are the required steps 
  #of the KS-test
  #Initialize the vectors to contain the calculated differences
  DnMinusSet=c()
  DnPlusSet=c()
  #calculate the absolute value of the differences
  for(i in 1:n){
    DnPlusSet[i]=abs((i/n)-pexp(data_sorted[i],lambdahat,lower.tail = TRUE,log.p=FALSE))
    DnMinusSet[i]=abs(pexp(data_sorted[i],lambdahat,lower.tail = TRUE,log.p=FALSE)-((i-1)/n))
  }
  # DnPlusSet
  # DnMinusSet
  DnPlus=max(DnPlusSet)
  DnMinus=max(DnMinusSet)
  Dn=max(DnMinus,DnPlus)
  # Dn
  #Calulate the adjusted D value relevant to the tested hypothesis.
  Dprimen=0
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
    Dprimen=(sqrt(n)+0.12+(0.11/sqrt(n)))*Dn
  } else if (row == 2) {
    if (confidence == 0.9) critical_value = 0.990
    else if (confidence == 0.95) critical_value =  1.094
    else if (confidence == 0.975) critical_value = 1.190
    else if (confidence == 0.99) critical_value = 1.308
    else {
          cat(sprintf("Throw error: confidence value(%f) is invalid\n", confidence))
          quit()
    }
    Dprimen=(Dn-(0.2/n))*(sqrt(n)+0.26+(0.5/sqrt(n)))
  } else {
    cat(sprintf("Throw error: Row value(%d) is invalid\n", row))
    quit()
  }
  # Dprimen
  #Compare the adjusted D value with the 
  #relevant critical value from the table 
  #on slide 9. (The hypothesis determines the
  #row from which the formula for the adjusted
  #D value AND the critical value are read )
  if (Dprimen>critical_value) {
    cat(sprintf("Dprimen: %f > critical_value: %f => TRUE\n", Dprimen, critical_value, Dprimen>critical_value))
  } else {
    cat(sprintf("Dprimen: %f <= critical_value: %f => FALSE\n", Dprimen, critical_value, Dprimen>critical_value))
  }
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
  Dprimen=0
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
    Dprimen=(sqrt(n)+0.12+(0.11/sqrt(n)))*Dn
  } else if (row == 1) {
    if (confidence == 0.9) critical_value = 0.819
    else if (confidence == 0.95) critical_value =  0.895
    else if (confidence == 0.975) critical_value = 0.955
    else if (confidence == 0.99) critical_value = 1.035
    else {
          cat(sprintf("Throw error: confidence value(%f) is invalid\n", confidence))
          quit()
    }
    Dprimen=(sqrt(n)-0.01+(0.85/sqrt(n)))*Dn
  } else {
    cat(sprintf("Throw error: Row value(%d) is invalid\n", row))
    quit()
  }
  # Dprimen
  #Compare the adjusted D value with the 
  #relevant critical value from the table 
  #on slide 9. (The hypothesis determines the
  #row from which the formula for the adjusted
  #D value AND the critical value are read )
  if (Dprimen>critical_value) {
    cat(sprintf("Dprimen: %f > critical_value: %f => TRUE\n", Dprimen, critical_value, Dprimen>critical_value))
  } else {
    cat(sprintf("Dprimen: %f <= critical_value: %f => FALSE\n", Dprimen, critical_value, Dprimen>critical_value))
  }
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
    # Dprimen=(Dn-(0.2/n))*(sqrt(n)+0.26+(0.5/sqrt(n)))

#1-----------------------------------------------------
#1 Q1
#1-----------------------------------------------------
# Question: Check if these(Q1data) inter-arrival times can be modelled by the exponential distribution.
cat("Q1:\n")
Q1data=scan("Tut9_data/Tut9_Q1.txt")
chi_exp(Q1data)
ks_exp(data=Q1data, confidence=0.95)

cat("\nQ2:\n")
Q2data=scan("Tut9_data/Tut9_Q2.txt")
chi_exp(Q2data, lambdahat=1/3)
ks_exp(data=Q2data, confidence=0.95, lambdahat=1/3)


cat("\nQ3:\n")
Q3data=scan("Tut9_data/Tut9_Q3.txt")
chi_norm(Q3data, mu=480, sigma=40)
ks_norm(data=Q3data, confidence=0.95, mu=480, sigma=40)


cat("\nQ4:\n")
Q4data=scan("Tut9_data/Tut9_Q4.txt")
chi_norm(Q4data)
ks_norm(data=Q4data, confidence=0.95)