P=matrix(c(
    0.80, 0.05, 0.15,
    0.04, 0.90, 0.06,
    0.06, 0.04, 0.90
), nrow=3, byrow=TRUE)
colnames(P) = c('urban', 'rural', 'suburban')
rownames(P) = c('urban', 'rural', 'suburban')

# P after year 1
P2 = P%*%P
# P after year 2
P3 = P%*%P2

# ------------------------------------
# PROBLEM (a) solution
# ------------------------------------

# initial position
x = c(1, 0, 0)

# condition after 2 years
x3 = x%*%P3
rownames(x3) = c('x3')


writeLines('\n--------------------')
writeLines('Problem (a) results:')
writeLines('--------------------\n')

writeLines('P = ')
print(P)

writeLines('\n\nAns=')
print(x3)

# ------------------------------------
# PROBLEM (b) solution
# ------------------------------------

# initial position
x = c(0.4, 0.25, 0.35)

# condition after 2 years
x3 = x%*%P3
rownames(x3) = c('x3')


writeLines('\n--------------------')
writeLines('Problem (b) results:')
writeLines('--------------------\n')

writeLines('P = ')
print(P)

writeLines('\n\nAns=')
print(x3)