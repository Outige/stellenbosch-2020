lambda = 18 # c/h
mu = 20 # c/h #1 # c/m = 3 min / c = 1/3 c/ 3m = 1 c/m

# Q1
cat(sprintf("Q1) %.4f\n", lambda))

# Q2
rho = lambda/mu
cat(sprintf("Q2) %.4f\n", rho))

# Q3 we are looking for Ls
Ls = rho
cat(sprintf("Q3) %.4f\n", Ls))

# Q4 we are looking for W (maybe Wq)
L = lambda/(mu-lambda)
W = (1/lambda)*L
cat(sprintf("Q4) %.4f\n", W))

# Q5 we are looking for: 1 - (Pi0 + Pi1 + Pi2 + Pi3 + Pi4)
Pi0 = 1-rho
Pic = c()
for (j in 0:4) Pic[j+1] = (rho^j)*Pi0

ans = 1-sum(Pic)
cat(sprintf("Q4) %.4f\n", ans))