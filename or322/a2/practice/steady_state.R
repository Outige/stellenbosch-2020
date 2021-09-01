n = nrow(P)
Pmod = P-diag(n)
Pmod[1:n, 1] = 1
PIj = c(1, rep(0, n-1))%*%solve(Pmod)