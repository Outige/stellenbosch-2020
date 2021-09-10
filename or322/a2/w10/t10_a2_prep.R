m = 2^31-1
a = 7^5
x = 987654321
c = 100000

Rs = c()

num = 520
for (i in 1:num) {
    x = (a*x+c)%%m
    R = x/m
    Rs[i] = R
}

get_expvar=function(R) {
    return(
        -(1/lambda)*log(1-R)
    )
}

lambda = 1/13

MyIATS = sapply(Rs, get_expvar)

MyIATS