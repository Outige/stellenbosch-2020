# STEADY STATE PROBABILITY & MEAN FIRST PASSAGE TIME CHAPTER 17
# -------------------------------------------------------------
source("/home/tieg/stellenbosch-2020/or322/a2/practice/steadylib.R")

# STEADY STATE PROBABILITY
# PIj?
test_PIj17(P, PIj)

# MEAN FIRST PASSAGE TIME
# Mij?
test_mfpt17(P, Mij)



# ABSORBING STATES & WORKFORCE PLANNING CHAPTER 17.6
# --------------------------------------------------
source("/home/tieg/stellenbosch-2020/or322/a2/practice/abslib.R") # import absorbing state P matrix

# ABSORBING STATES
# index?
# A?
# F?
test_absorbing17(P, index, F, A)

# WORKFORCE PLANNING
# hbar? (one will be given)
# bbar?
test_wfp17(P, index, hbar, bbar)


# OPEN QUEUE CHAPTER 20.10
# ------------------------
source("/home/tieg/stellenbosch-2020/or322/a2/practice/openlib.R")

# lambdabar?
test_open10(P, rbar, lambdabar)




# CLOSED QUEUE CHAPTER 20.13
# --------------------------
source("/home/tieg/stellenbosch-2020/or322/a2/practice/closedlib.R")

# lambdabar?
test_closed13(P, lambdabar)