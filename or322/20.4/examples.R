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