P=matrix(c(
    1.0, 0.0, 0.0, 0.0, 0.0,
    0.5, 0.0, 0.5, 0.0, 0.0,
    0.0, 0.5, 0.0, 0.5, 0.0,
    0.0, 0.0, 0.5, 0.0, 0.5,
    0.0, 0.0, 0.0, 0.0, 1.0
), nrow=5, byrow=TRUE)
colnames(P) = c(0, 1, 2, 3, 4)
rownames(P) = 0:4

# P2 = MatrixMult(P, P)
P2 = P%*%P
P3 = P%*%P2

# bara
bara = c(0,0,1,0,0)
bara%*%P2
bara%*%P3