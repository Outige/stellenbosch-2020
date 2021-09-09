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

test_closed13 = function(P, lambdabar) {
  lambdabar_ = get_lambdabar13(P)
  lambdabar = matrix(c(lambdabar), nrow=1) # need to type cast vector to matrix
  lambdabar_ = matrix(c(lambdabar_), nrow=1)
  stopifnot(test_equal(lambdabar, lambdabar_))
}