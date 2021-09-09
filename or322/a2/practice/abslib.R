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
colnames(P) = rownames(P) = c('F.', 'So.', 'J.', 'Sen.', 'Q.', 'G.')

# ABSORBING STATE CHAPTER 17
# --------------------------
# NOTE: for all the fowllowing functions index refers
#       to the index of the 1st absorbing state

get_F17 = function(P, index) {
    Q = P[1:index, 1:index]
    return(solve(diag(nrow(Q)) - Q))
}

get_A17 = function(P, index) {
    Q = P[1:index, 1:index]
    R = P[1:index, (index+1):nrow(P)]
    F = get_F17(P, index)
    return(F%*%R)
}

test_absorbing17 = function(P, index, F, A) {
    Ftest = get_F17(P, index)
    Atest = get_A17(P, index)
    
    stopifnot(length(F) == length(Ftest))
    for (i in 1:nrow(F)) {
        for (j in 1:ncol(F)) {
            stopifnot(F[i, j] == Ftest[i, j])
        }
    }

    stopifnot(length(A) == length(Atest))
    for (i in 1:nrow(A)) { # every index equal
        for (j in 1:ncol(A)) {
            stopifnot(A[i, j] == Atest[i, j])
        }
    }
    for (i in 1:nrow(A)) { # all rows sum to 1
        stopifnot(sum(A[i,]) == 1)
    }
}