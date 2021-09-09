N=S=3
mubar = c(
    1/0.039,
    1/0.18,
    1/0.26
)
P = matrix(c(
    1/20, 13/20, 6/20,
    1, 0, 0,
    1, 0, 0
), nrow=3, byrow=TRUE)
colnames(P) = rownames(P) = c("CPU", "D1", "D2")

get_lambdabar13 = function(P) {
    n = nrow(P)
    A = diag(n) - t(P)
    LK = A[2:n, 2:n]
    RK = -1*A[2:n, 1]
    return(c(1, solve(LK)%*%RK))
}
lambdabar = get_lambdabar13(P)
rhobar = lambdabar/mubar

get_all_states = function(N, S) {
  install.packages("partitions")
  library(partitions)
  return(t(compositions(N,S)))
}
all_states = get_all_states(N,S)

get_tau = function(N, S) {
  return(choose(N+S-1, S-1))
}
tau = get_tau(N, S)

get_GN = function(tau, S, rhobar, all_states) {
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
GN = get_GN(tau, S, rhobar, all_states)
GN

get_PIbar = function(tau, S, rhobar, all_states) {
  GN = get_GN(tau, S, rhobar, all_states)
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
PIbar=get_PIbar(tau, S, rhobar, all_states)
PIbar
sum(PIbar)

gamma = function(s, eta, tau, all_states, PIbar) {
  out = 0
  for (i in 1:tau) {
    if (all_states[i, s] == eta) {
      out = out + PIbar[i]
    }
  }
  return(out)
}

get_PIj = function(S, N, tau, all_states, PIbar, PIj) {
  for (s in 1:S) {
    for (eta in 0:N) {
      PIj[s, eta+1] = gamma(s, eta, tau, all_states, PIbar)
    }
  }
  return(PIj)
}

PIj = matrix(c(
  0, 0, 0, 0,
  0, 0, 0, 0,
  0, 0, 0, 0
), nrow=3, byrow=TRUE)
colnames(PIj) = c(0, 1, 2, 3)
rownames(PIj) = c("CPU", "D1", "D2")
PIj = get_PIj(S, N, tau, all_states, PIbar, PIj)