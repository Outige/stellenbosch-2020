source("/home/tieg/stellenbosch-2020/or322/a2/practice/testutils.R")

N = 3 # number of stations
mubar = c(
    1/0.039, # jobs/second (mu must be unit/time)
    1/0.180, # jobs/second
    1/0.260  # jobs/second

) # service times from the question
P = matrix(c(
    1/20, 13/20, 6/20,
    1, 0, 0,
    1, 0, 0
), nrow=3, byrow=TRUE) # transition probability martix from the question
rownames(P) = colnames(P) = c("CPU", "D1", "D2")
S = 3 # 3 jobs

# CLOSED QUEUE CHAPTER 20.13
# --------------------------
get_lambdabar13 = function(P) {
  N = ncol(P)
  A = diag(N)-t(P)
  LK = A[2:N,2:N]
  RK = -1*A[2:N,1]
  lambda_other = solve(LK)%*%RK
  return(c(1, lambda_other))
}

get_rhobar13 = function(lambdabar, mubar) {
  return(lambdabar/mubar)
}

get_all_states13 = function(N, S) {
  stopifnot(N == 3)
  stopifnot(S == 3)
  return(
    matrix(c(
      3, 0, 0,
      2, 1, 0,
      1, 2, 0,
      0, 3, 0,
      2, 0, 1,
      1, 1, 1,
      0, 2, 1,
      1, 0, 2,
      0, 1, 2,
      0, 0, 3), nrow=10, byrow=TRUE)
  )
}

get_tau13 = function(N, S) {
  return(choose(N+S-1, S-1))
}

get_GN13 = function(tau, S, rhobar, all_states) {
  GN = 0
  for (i in 1:tau) {
    prod = 1
    for (j in 1:S) {
      prod = prod*rhobar[j]^all_states[i, j]
    }
    GN = GN + prod
  }
  return(GN)
}

get_PIbar13 = function(tau, S, rhobar, all_states) {
  GN = get_GN13(tau, S, rhobar, all_states)
  PIbar = c()
  for (i in 1:tau) {
    prod = 1
    for (j in 1:S) {
      prod = prod*rhobar[j]^all_states[i, j]
    }
    PIbar[i] = prod/GN
  }
  return(PIbar)
}

gamma13 = function(s, eta, tau, all_states, PIbar) {
  out = 0
  for (i in 1:tau) {
    if (all_states[i, s] == eta) {
      out = out + PIbar[i]
    }
  }
  return(out)
}

get_PIj13 = function(S, N, tau, all_states, PIbar) {
  PIj = matrix(c(rep(0, (N+1)*S)), nrow=S)
  for (s in 1:S) {
    for (eta in 0:N) {
      PIj[s, eta+1] = gamma13(s, eta, tau, all_states, PIbar)
    }
  }
  return(PIj)
}

test_closed13 = function(P, mubar, lambdabar, rhobar, all_states, tau, GN) {
  lambdabar_ = get_lambdabar13(P)
  lambdabar = matrix(c(lambdabar), nrow=1) # need to type cast vector to matrix
  lambdabar_ = matrix(c(lambdabar_), nrow=1)
  stopifnot(test_equal(lambdabar, lambdabar_))

  rhobar_ = get_rhobar13(lambdabar_, mubar)
  rhobar_ = matrix(rhobar_, nrow=1)
  rhobar = matrix(rhobar, nrow=1)
  stopifnot(test_equal(rhobar, rhobar_))

  all_states_ = get_all_states13(N, S)
  stopifnot(test_equal(all_states, all_states_))

  tau_ = get_tau13(N, S)
  stopifnot(tau == tau_)

  GN_ = get_GN13(tau_, S, rhobar_, all_states_)
  stopifnot(GN == GN_)

  PIbar_ = get_PIbar13(tau_, S, rhobar_, all_states_)
  PIbar = matrix(PIbar, nrow=1)
  PIbar_ = matrix(PIbar_, nrow=1)
  stopifnot(test_equal(PIbar, PIbar_))

  PIj_ = get_PIj13(S, N, tau_, all_states_, PIbar_)
  stopifnot(test_equal(PIj, PIj_))
}