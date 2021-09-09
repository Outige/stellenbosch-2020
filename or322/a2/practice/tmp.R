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
    2,7,0,3,0,
    1,1,7,2,1,
    1,0,1,1,9,
    0,0,0,1,0,
    0,0,0,0,1
), nrow=5, byrow=TRUE)
colnames(P)=c('1', '2', '3', 'V', 'G')
rownames(P)=c('1', '2', '3', 'V', 'G')
Q = P[1:3, 1:3]
Q

hbar =
bbar = c(500, 450, 400)




# OPEN QUEUE CHAPTER 20.10
# ------------------------
#



# CLOSED QUEUE CHAPTER 20.13
# --------------------------
#