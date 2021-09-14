tolerance = 0.000000000001 # had to hardcode a tolerance for workforce planning

test_equal = function(A, B) {
    if (nrow(A) != nrow(B)) return(FALSE)
    if (ncol(A) != ncol(B)) return(FALSE)
    for (i in 1:nrow(A)) {
        for (j in 1:ncol(A)) {
            if (abs(A[i,j] - B[i,j]) > tolerance) {
                cat(sprintf("error index [%d, %d]\n", i, j))
                return(FALSE)
            }
        }
    }
    return(TRUE)
}

test_row_sum = function(A, value) {
    for (i in 1:nrow(A)) {
        if (sum(A[i,]) != value) return(FALSE)
    }
    return(TRUE)
}