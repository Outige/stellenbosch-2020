source("/home/tieg/stellenbosch-2020/or322/a2/practice/testutils.R")

rbar = c(
  10, # jobs/hour
  0, # jobs/hour
  0 # jobs/hour
)

mubar = c(
  20, # jobs/hour
  20, # jobs/hour
  20 # jobs/hour
)

P = matrix(c(
  0,   1,   0,
  0.1, 0,   0.9,
  0,   0.2, 0
), nrow=3, byrow=TRUE)

# OPEN QUEUE CHAPTER 20.10
# ------------------------
get_lambdabar10 = function(P, rbar) {
    return(
        solve(diag(ncol(P))-t(P))%*%rbar
    )
}

test_open10 = function(P, rbar, lambdabar) {
    lambdabar_ = get_lambdabar10(P, rbar)
    stopifnot(test_equal(lambdabar, lambdabar_))
}