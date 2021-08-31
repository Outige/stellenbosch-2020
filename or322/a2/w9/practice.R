chi_exp = function(data, lambdahat=-1) {
  k = ceiling(log(length(data), 2)) + 1
  
  r = 0
  if (lambdahat == -1) { # approximate lambda
    lambdahat = 1/mean(data)
    r = r+1
  }

  ub = c()
  for (i in 1:(k-1)) {
    ub[i] = (-1/lambdahat)*log(1-i/k)
  }
  ub[k] = max(data)+1

  lb = c(0)
  for (i in 2:(k)) {
    lb[i] = ub[i-1]
  }

  cat(sprintf("k: %d\nlambdahat: %f\nr: %d\nub: %s\nlb: %s\n",
  k, lambdahat, r, paste(ub,collapse=" "), paste(lb,collapse=" ")))
  return(0)
}

data=scan("Tut9_data/Tut9_Q1.txt")
chi_exp(data)