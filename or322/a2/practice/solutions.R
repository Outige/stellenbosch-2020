# STEADY STATE PROBABILITY & MEAN FIRST PASSAGE TIME CHAPTER 17
# -------------------------------------------------------------
source("/home/tieg/stellenbosch-2020/or322/a2/practice/steadylib.R")

# STEADY STATE PROBABILITY
Pmod = P-diag(n)
Pmod[,1] = 1
Pmod = solve(Pmod)
PIj = c(1,0,0,0,0)%*%Pmod
test_PIj17(P, PIj)

# MEAN FIRST PASSAGE TIME
Mij = matrix(rep(rep(0, 4), 4), nrow=4)
Mij[1,1] = 1/PIj[1]
Mij[2,2] = 1/PIj[2]
Mij[3,3] = 1/PIj[3]
Mij[4,4] = 1/PIj[4]
test_mfpt17(P, Mij)



# ABSORBING STATES & WORKFORCE PLANNING CHAPTER 17.6
# --------------------------------------------------
source("/home/tieg/stellenbosch-2020/or322/a2/practice/abslib.R") # import absorbing state P matrix

# ABSORBING STATES
Q = P[1:4, 1:4]
R = P[1:4, 5:6]
F = solve(diag(nrow(Q)) - Q)
A = F%*%R
test_absorbing17(P, index=4, F, A)

# WORKFORCE PLANNING
Q = P[1:4, 1:4]
hbar = c(7000, 500, 500, 0)
bbar = hbar%*%solve(diag(nrow(Q))-Q)
hbar = bbar%*%(diag(nrow(Q))-Q)
test_wfp17(P, index=4, hbar, bbar)


# OPEN QUEUE CHAPTER 20.10
# ------------------------
source("/home/tieg/stellenbosch-2020/or322/a2/practice/openlib.R")

lambdabar = solve(diag(ncol(P))-t(P))%*%rbar
test_open10(P, rbar, lambdabar)




# CLOSED QUEUE CHAPTER 20.13
# --------------------------
source("/home/tieg/stellenbosch-2020/or322/a2/practice/closedlib.R")

A = diag(3) - t(P)
LK = A[2:3, 2:3]
RK = -1*A[2:3, 1]
lambdabar = c(1, solve(LK)%*%RK)
test_closed13(P, lambdabar)



# KS & KS METHOD 20.12
# --------------------
source("/home/tieg/stellenbosch-2020/or322/a2/practice/kscslib.R")
# NOTE: I am not good enough at this to have solution code. We only have "memo" code

test_ks_exp12(data=d1, result=ks_exp12(d1), test_result=ks_test12(ks_exp12(d1)))
test_ks_norm12(data=d1, result=ks_norm12(d1), test_result=ks_test12(ks_norm12(d1)))