
S=c('V','R','K','L')
P=matrix(c(1/3,1/3,0,0,0,1/3,2/3,0,1/3,1/3,1/3,0,1/3,0,0,1),ncol=4,dimnames=list(S,S))
P
Q=P[1:3,1:3]
Q
bbar=c(5,5,5)
# h_1=(bbar[1]*(1-P[1,1]))-((bbar[2]*P[2,1])+(bbar[3]*P[3,1]))
h_1
hbar=bbar%*%(diag(3)-Q)
hbar
hbar=c(y,0,0)
#bbar=hbar%*%solve(diag(4)-Q)
#bbar
