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



# ABSORBING STATES CHAPTER 17
# ---------------------------
source("/home/tieg/stellenbosch-2020/or322/a2/practice/abslib.R") # import absorbing state P matrix
Q = P[1:4, 1:4]
R = P[1:4, 5:6]
F = solve(diag(nrow(Q)) - Q)
A = F%*%R
test_absorbing17(P, index=4, F, A)



# WORKFORCE PLANNING CHAPTER 17
# -----------------------------
P = matrix(c(
    0.10, 0.80, 0.00, 0.00, 0.10, 0.00,
    0.00, 0.10, 0.85, 0.00, 0.05, 0.00,
    0.00, 0.00, 0.15, 0.80, 0.05, 0.00,
    0.00, 0.00, 0.00, 0.10, 0.05, 0.85,
    0.00, 0.00, 0.00, 0.00, 1.00, 0.00,
    0.00, 0.00, 0.00, 0.00, 0.00, 1.00
), nrow=6, byrow=TRUE)
colnames(P) = rownames(P) = c("F.", "So.", "J.", "Sen.", "Q.", "G.")
Q = P[1:4, 1:4]

hbar = c(7000, 500, 500, 0)
bbar = hbar%*%solve(diag(nrow(Q))-Q)
bbar

hbar = bbar%*%(diag(nrow(Q))-Q)
hbar




# OPEN QUEUE CHAPTER 20.10
# ------------------------
#



# CLOSED QUEUE CHAPTER 20.13
# --------------------------
#