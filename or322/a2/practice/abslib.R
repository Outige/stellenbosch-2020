source("/home/tieg/stellenbosch-2020/or322/a2/practice/testutils.R")

P = matrix(c(
    0.10, 0.80, 0.00, 0.00, 0.10, 0.00,
    0.00, 0.10, 0.85, 0.00, 0.05, 0.00,
    0.00, 0.00, 0.15, 0.80, 0.05, 0.00,
    0.00, 0.00, 0.00, 0.10, 0.05, 0.85,
    0.00, 0.00, 0.00, 0.00, 1.00, 0.00,
    0.00, 0.00, 0.00, 0.00, 0.00, 1.00
), nrow=6, byrow=TRUE)
colnames(P) = rownames(P) = c("F.", "So.", "J.", "Sen.", "Q.", "G.")

# ABSORBING STATE CHAPTER 17.6
# ----------------------------
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
    F_ = get_F17(P, index)
    A_ = get_A17(P, index)

    stopifnot(test_equal(F, F_))

    stopifnot(test_equal(A, A_))
    stopifnot(test_row_sum(A, 1))
}

# WORKFORCE PLANNING CHAPTER 17.6
# -------------------------------
get_bbar17 = function(P, index, hbar) {
    Q = P[1:index, 1:index]
    return(hbar%*%solve(diag(nrow(Q))-Q))
}

get_hbar17 = function(P, index, bbar) {
    return(bbar%*%(diag(nrow(Q))-Q))
}

test_wfp17 = function(P, index, hbar, bbar) {
    hbar_ = get_hbar17(P, index, bbar)
    bbar_ = get_bbar17(P, index, hbar)
    
    bbar = matrix(bbar, nrow=1)
    hbar = matrix(hbar, nrow=1)
    stopifnot(test_equal(hbar, hbar_))
    stopifnot(test_equal(bbar, bbar_))
}