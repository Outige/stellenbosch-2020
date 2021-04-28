#0-------------------------------------
#0 QUESTION 1
#0-------------------------------------
cat('[P]\n')
P = matrix(
    c(
        0.1, 0.8, 0, 0, 0.1, 0,
        0, 0.1, .85, 0, 0.05, 0,
        0, 0, 0.15, 0.8, 0.05, 0,
        0, 0, 0, 0.1, 0.05, 0.85,
        0, 0, 0, 0, 1, 0,
        0, 0, 0, 0, 0, 1
    ), nrow=6, byrow=TRUE
)
colnames(P) = c('F.', 'So.', 'J.', 'Sen.', 'Q.', 'G.')
rownames(P) = c('F.', 'So.', 'J.', 'Sen.', 'Q.', 'G.')
P

cat('\n[Q]\n')
Q = P[1:4, 1:4]
Q

cat('\n[R]\n')
R = P[1:4, 5:6]
R

#1---
#1 1a
#1---
# We want to pre-calculate calculate F or Inverse(I - Q)
# F[i][j] = the amount of time you will spend in state j if you started in state i
cat('\n[F or Inverse(I - Q)]\n')
F = solve(diag(4) - Q)
F


# So we are looking for Sum(F[i=1]) = The amount of time as a
# F., So., J., Sen. Given that we started as a F.
ans = sum(F[1,])
cat(sprintf("\n1a) %.4f years\n", ans))

#1---
#1 1b
#1---
# We want to pre-calculate A or Inverse(I - Q)*R
cat('\n[A or Inverse(I - Q)*R\n')
A = F%*%R
A

# A[i][j] is the probability of starting in trainsiant
# state i and ending up in absorbing satete j
ans = A[1,2]
cat(sprintf("\n1b) %.4f\n", ans))