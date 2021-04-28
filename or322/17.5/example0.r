# # S={0, 1, 2} represents the number of working machines at the beggining of the day
# P=matrix(c(
#     0.0, 0.0, 1.0,
#     0.0, 1/3, 2/3,
#     1/9, 4/9, 4/9
# ), nrow=3, byrow=TRUE)
# colnames(P) = c('0', '1', '2')
# rownames(P) = c('0', '1', '2')

# # Print P
# P

# # 1st we modify P. P = P - I
# PMod = P - diag(3)

# # Replace any column with 1s. We choose the last one here
# PMod[,3] = 1
# PMod

# # Invert PMod
# PModInv = solve(PMod)
# PModInv

# # Calculate pibar. 0s except for the column of PMod that is all 1s
# pibar = c(0, 0, 1)%*%PModInv
# pibar

# (17.5 P4)
P = matrix(c(
    0.85, 0.1, 0.05,
    0, 0.7, 0.3,
    1, 0, 0
), nrow=3, byrow=TRUE)
colnames(P) = c('G', 'F', 'B')
rownames(P) = c('G', 'F', 'B')
P

PMod = P - diag(3)

# Replace any column with 1s. We choose the last one here
PMod[,3] = 1
PMod

# Invert PMod
PModInv = solve(PMod)
PModInv

# Calculate pibar. 0s except for the column of PMod that is all 1s
pibar = c(0, 0, 1)%*%PModInv
pibar