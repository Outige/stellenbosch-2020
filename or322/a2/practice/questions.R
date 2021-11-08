# STEADY STATE PROBABILITY & MEAN FIRST PASSAGE TIME CHAPTER 17
# -------------------------------------------------------------
source("/home/tieg/stellenbosch-2020/or322/a2/practice/steadylib.R")

# STEADY STATE PROBABILITY
#
test_PIj17(P, PIj)

# MEAN FIRST PASSAGE TIME
#
test_mfpt17(P, Mij)



# ABSORBING STATES & WORKFORCE PLANNING CHAPTER 17.6
# --------------------------------------------------
source("/home/tieg/stellenbosch-2020/or322/a2/practice/abslib.R") # import absorbing state P matrix

# ABSORBING STATES
#
test_absorbing17(P, index=4, F, A)

# WORKFORCE PLANNING
#
test_wfp17(P, index=4, hbar, bbar)


# OPEN QUEUE CHAPTER 20.10
# ------------------------
source("/home/tieg/stellenbosch-2020/or322/a2/practice/openlib.R")

#
test_open10(P, rbar, lambdabar)




# CLOSED QUEUE CHAPTER 20.13 # NOTE: hard coded to N=S=3
# --------------------------
source("/home/tieg/stellenbosch-2020/or322/a2/practice/closedlib.R")

#

all_states = get_all_states13(N, S)

#

test_closed13(P, mubar, lambdabar, rhobar, all_states, tau, GN)

# RANDOM NUMBERS 21.3
# -------------------
source("/home/tieg/stellenbosch-2020/or322/a2/practice/randomlib.R")

# EXP
#

test_exp_arrival_times13(random_numbers, random_variables, arrival_times, n, seed, c, a, m, lambda, t0)

# POIS
#
test_pois13(random_numbers, random_variables, n, seed, c, a, m, lambda)

# UNIFORM
vmin = 90/60
vmax = 300/60
#
test_uni13(random_numbers, random_variables, n, seed, c, a, m, lambda, vmin, vmax)

# BERNOULLI - REMEMBER ME

# BINOMEAL - REMEMBER ME

# KS & KS METHOD 20.12
# --------------------
source("/home/tieg/stellenbosch-2020/or322/a2/practice/kscslib.R")
# NOTE: I am not good enough at this to have solution code. We only have "memo" code

# test_ks_exp12(data=d1, result=ks_exp12(d1), test_result=ks_test12(ks_exp12(d1)))
# test_ks_norm12(data=d1, result=ks_norm12(d1), test_result=ks_test12(ks_norm12(d1)))