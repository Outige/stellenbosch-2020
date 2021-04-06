P=matrix(c(
    0.1, 0.8, 0, 0, 0.1, 0,
    0, 0.1, 0.85, 0, 0.05, 0,
    0, 0, 0.15, 0.8, 0.05, 0,
    0, 0, 0, 0.1, 0.05, 0.85,
    0, 0, 0, 0, 1, 0,
    0, 0, 0, 0, 0, 1
), nrow=6, byrow=TRUE)
colnames(P) = c("F.", "So.", "J.", "Sen.", "Q.", "G.")
rownames(P) = c("F.", "So.", "J.", "Sen.", "Q.", "G.")
P

Q=P[1:4,1:4]
Q

F=solve(diag(4)-Q)
F

hbar=c(7000, 500, 500, 0)
bbar=hbar%*%F
bbar

# FIXME how to get hbar with R? It isn't: hbar=bbar%*%F