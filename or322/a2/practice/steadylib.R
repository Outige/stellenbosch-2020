source("/home/tieg/stellenbosch-2020/or322/a2/practice/testutils.R")

P = matrix(c(
    0.1, 0.4, 0.1, 0.2, 0.2,
    0.3, 0.4, 0.1, 0.1, 0.1,
    0.2, 0, 0.1, 0.6, 0.1,
    0.1, 0.1, 0.1, 0.1, 0.6,
    0.2, 0.5, 0.2, 0, 0.1
), nrow=5, byrow=TRUE)
colnames(P) = rownames(P) = 1:5
n = 5

# STEADY STATE CHAPTER 17
# -----------------------
get_PIj17 = function(P) {
    n = nrow(P)
    Pmod = P-diag(n)
    Pmod[1:n, 1] = 1
    return(
        c(1, rep(0, n-1))%*%solve(Pmod)
    )
}

test_PIj17 = function(P, PIj) {
    PIj_ = get_PIj17(P)
    stopifnot(test_equal(PIj, PIj_))
    stopifnot(test_row_sum(PIj, 1))
    stopifnot(test_row_sum(PIj_, 1))
}

# MEAN FIRST PASSAGE TIME CHAPTER 17
# ----------------------------------
get_mfpt17 = function(P, i, j) {
    PIj = get_PIj17(P)
    if (i == j) {
        return(1/PIj[i])
    } else {
        cat("ERROR: Mij (NOTE: Mii is fine)\n")
        quit()
    }
}

test_mfpt17 = function(P, Mij) {
    cat("TODO: Mij (NOTE: Mii is fine)\n")
    for (i in 1:nrow(Mij)) {
        for (j in 1:ncol(Mij)) {
            if (i == j) {
                stopifnot(Mij[i,j] == get_mfpt17(P, i, j))
            }
        }
    }
}