##Tool centre
lambda=10 #m/h
mu=1/5*60 #m/min*h
mu_h=1/4*60 #m/min*h
#c_curr=c_c+L*c_m
#c_alt=c_c+c_h+L_alt*c_m
L=lambda/(mu-lambda)
c_c=6 #$/h
c_m=10 #$/h
c_h=4 #$/h
c_curr=c_c+c_m*L
#alt
L_alt=lambda/(mu_h-lambda)
c_alt=c_c+c_h+c_m*L_alt
savings=c_curr-c_alt

##no.1 pg. 1081
#a
lambda=10
mu=12
rho=lambda/mu
pi_0=1-rho
pi_1=rho*pi_0
1-pi_0-pi_1
#b
Lq=(rho^2)/pi_0
#c
W=L/lambda


##20.5.7
#How many people should be put on hold (c-1) such that at most 1% of calls are lost?
lambda=60 #calls/h
mu=1*60 #calls/h
rho=lambda/mu
c=5
pi_c=1/(c+1)
pi_c=function(c)return(1/(c+1))
pi_c(9)
for(c in 1:10){if pi_c(c)<=0.1{
  cat("for c=",c,"there will be ",c-1,"people on hold and ",pi_c(c),"callers will be lost")
  break
  }}
pi_c(8)
pi_c(10)

##20.5.1
lambda=3 #c/h
mu=2 #c/h
c=3 #customers
rho=lambda/mu
pi_0=(1-rho)/(1-rho^4)
pi_c=(rho^3)*pi_0
lambdabar=lambda*(1-pi_c)
#b
1-pi_0
