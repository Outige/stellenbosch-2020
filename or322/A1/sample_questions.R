# #0------------------------------------------
# #0 QUESTION 7
# #0------------------------------------------
# cat("\n\nQ7\n")
# # We have an M/M/1/GD/INF/INF queue
# lambda = 225/8 # c/h = 1c/128s
# mu = 30 # c/h
# rho = lambda/mu

# #1----
# #1 7.1
# #1----
# # We are looking for Pi0
# Pi0 = 1-rho
# cat(sprintf("7.1) Pi0: %.4f\n", Pi0))

# #1----
# #1 7.2
# #1----
# # We are looking for Pi3
# j=3
# Pi3 = (rho^j)*Pi0
# cat(sprintf("7.2) Pi3: %.4f\n", Pi3))

# #1----
# #1 7.3
# #1----
# # We are looking for L
# L = lambda/(mu-lambda)
# cat(sprintf("7.3) L: %.1f\n", L))

# #1----
# #1 7.4
# #1----
# # We are looking for W
# W = (1/lambda)*L
# cat(sprintf("7.4) W: %.4f\n", W))





# #0------------------------------------------
# #0 QUESTION 4
# #0------------------------------------------
# cat("\n\nQ4\n")
# # We have an M/M/1/GD/5/INF queue
# c = 5
# lambda = 0.1*500 # c/h
# mu = 15 # c/h = 4 min/c = 1/4 c/min
# rho = lambda/mu

# #1----
# #1 4.1
# #1----
# # We are looking for L
# L = ( rho*(1-(rho^c)*(1+c) + c*(rho^(c+1))) ) / ( ( 1 - rho^(c+1) ) * ( 1 - rho ) )
# cat(sprintf("4.1) L: %.4f customers\n", L))

# #1----
# #1 4.2
# #1----
# # My first though would be to calculate Ls. The average number of people
# # getting served per hour, but from doing examples I think I must caluclate
# # "effective lambda"
# Pi0 = (1-rho)/(1-rho^(c+1))
# Pic = rho^(c)*Pi0
# lambda_effective = lambda*(1-Pic)
# cat(sprintf("4.2) Effective Lambda: %.4f c/h\n", lambda_effective))

# #1----
# #1 4.3
# #1----
# # We are looking for W
# W = (1/lambda_effective)*L
# cat(sprintf("4.3) W: %.4f h\n", W))

# #1----
# #1 4.4
# #1----

#0------------------------------------------
#0 QUESTION 3
#0------------------------------------------
P = matrix(c(
    0.1, 0.4, 0.1, 0.2, 0.2,
    0.3, 0.4, 0.1, 0.1, 0.1,
    0.2, 0, 0.1, 0.6, 0.1,
    0.1, 0.1, 0.1, 0.1, 0.6,
    0.2, 0.5, 0.2, 0, 0.1
), nrow=5, byrow=TRUE)
colnames(P) = 1:5
rownames(P) = 1:5

#1----
#1 3.1
#1----
# What we have to do if raise our matrix to the power of the number
# of jumps in the future. We have 3 jumps in the future.
# Then on this new matrix the row is where we state at original t
# and the column is where we end in current time
P3 = P%*%P%*%P
ans = P3[1, 4]
cat(sprintf("3.1) %.4f\n", ans))

#1----
#1 3.2
#1----
# So we want to do this to P:
# (1) subtract I
# (2) Replace any column (but remember which col) with all 1's
# (3) Inverse Pmod
# to recap: Pmod = Inverse((P-I)[,i] = 1)
one_col = 5
I = diag(5)
Pmod = (P - I)
Pmod[,one_col] = 1
Pmod = solve(Pmod)

# Then we want to solve for steady state distribution.
# We pre-multiply by an all 0 vector, with a 1 in the index of the all 1 column in Pmod
Pij = c(0,0,0,0,1)%*%Pmod
rownames(Pij) = c('3.2)')
Pij

#1----
#1 3.3
#1----
# We have already calculated our Pij.
# Where the index is the portion of the time we will spend in that state on average
# So all we have to do is index into Pij by the state we require
ans = Pij[3]
cat(sprintf("3.3) %.4f\n", ans))

#1----
#1 3.4
#1----
# Here we want to calculate M11
# Mii = 1/steady_state(i)
# mean first passage time is: on average how many
# transitions it will take us to get to a state from a state
M11 = 1/(Pij[1])
cat(sprintf("3.4) %.4f\n", M11))