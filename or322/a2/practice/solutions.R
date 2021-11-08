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




# CLOSED QUEUE CHAPTER 20.13 # NOTE: hard coded to N=S=3
# --------------------------
source("/home/tieg/stellenbosch-2020/or322/a2/practice/closedlib.R")

A = diag(3) - t(P)
LK = A[2:3, 2:3]
RK = -1*A[2:3, 1]
lambdabar = c(1, solve(LK)%*%RK)

rhobar = lambdabar/mubar

all_states = get_all_states13(N, S)

tau = choose(N+S-1,S-1)


GN = 0
for (i in 1:tau) {
  prod = 1
  for (j in 1:S) {
    prod = prod*rhobar[j]^(all_states[i,j])
  }
  GN = GN + prod
}

PIbar = c()
for (i in 1:tau) {
  prod = 1
  for (j in 1:S) {
    prod = prod*rhobar[j]^(all_states[i,j])
  }
  PIbar[i] = prod/GN
}

gamma = function(s, eta) {
  out = 0
  for (i in 1:tau) {
    if (all_states[i,s] == eta) {
      out = out + PIbar[i]
    }
  }
  return(out)
}

PIj = matrix(c(rep(0, 12)), nrow=3, byrow=TRUE)
for (s in 1:S) {
  for (eta in 0:N) {
    PIj[s,eta+1] = gamma(s, eta)
  }
}
colnames(PIj) = c(0, 1, 2, 3)

test_closed13(P, mubar, lambdabar, rhobar, all_states, tau, GN)

# RANDOM NUMBERS 21.3
# -------------------
source("/home/tieg/stellenbosch-2020/or322/a2/practice/randomlib.R")

# EXP
random_numbers = c()
for (i in 1:n) {
    x = (a*x+c)%%m
    random_numbers[i] = x/m
}

random_variables = qexp(random_numbers, lambda)

arrival_times = c(t0)
for (i in 2:length(random_variables)) {
    arrival_times[i] = sum(random_variables[1:i-1]) + t0
}

test_exp_arrival_times13(random_numbers, random_variables, arrival_times, n, seed, c, a, m, lambda, t0)

# POIS
random_variables = qpois(random_numbers, lambda)
test_pois13(random_numbers, random_variables, n, seed, c, a, m, lambda)

# UNIFORM
vmin = 90/60
vmax = 300/60
random_variables = c()
for (i in 1:length(random_numbers)) {
  random_variables[i] = vmin + (vmax-vmin)*random_numbers[i]
}
test_uni13(random_numbers, random_variables, n, seed, c, a, m, lambda, vmin, vmax)

# BERNOULLI
p = 0.3
x = seed
random_numbers = get_random_numbers3(n, x, c)
bernoulli = c()
for (i in 1:n) {
    if (random_numbers[i] <= p) {
        bernoulli[i] = 1
    } else {
        bernoulli[i] = 0
    }
}

x = seed
test_bernoulli13(bernoulli, n, x, c, a, m, p)


# BERNOULLI
ntests = 200

upadte_seed = function(n, x, c, a, m) {
    for (i in 1:n) {
        x = (a*x + c)%%m
    }
    return(x)
}

binomial = matrix(c(rep(0, ntests*n)), nrow=200)
for (test in 1:ntests) {
    random_numbers = get_random_numbers3(n, x, c)
    x = upadte_seed(n, x, c, a, m)
    for (i in 1:n) {
        if (random_numbers[i] <= p) {
            random_numbers[i] = 1
        } else {
            random_numbers[i] = 0
        }
    }
    binomial[test,] = random_numbers 
}
test_binomial13(binomial, n, seed, c, a, m, p, ntests)

# KS & KS METHOD 20.12
# --------------------
source("/home/tieg/stellenbosch-2020/or322/a2/practice/kscslib.R")
# NOTE: I am not good enough at this to have solution code. We only have "memo" code

# test_ks_exp12(data=d1, result=ks_exp12(d1), test_result=ks_test12(ks_exp12(d1)))
# test_ks_norm12(data=d1, result=ks_norm12(d1), test_result=ks_test12(ks_norm12(d1)))

