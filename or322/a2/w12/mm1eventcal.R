sv=data.frame(0,0,0)
names(sv)=c("queueL","serverB","stateDuration")

statevector=c(0,0,0)

eventtimes=c(0)
eventnames=c("Arrival")

eventcal = data.frame(eventtimes,eventnames)

pass = 0

setruntime=1000

m=2^31 -1
a=7^5
c=100100100
x=987654321

lambda=1/11

mu = 1/10

timenow=0

lastchangeTime = 0

while(timenow < setruntime){
  x = (a*x+c)%%m
  R=x/m
  if(eventcal[1,2]=="Arrival"){
    timenow=eventtimes[1]
    eventtimes=append(eventtimes,timenow+(-1/lambda)*log(1-R))
    eventnames=append(eventnames,"Arrival")

    duration=timenow-lastchangeTime
    statevector[3]=duration
    sv=rbind(sv,statevector)

    statevector[1]=statevector[1]+1
    lastchangeTime=timenow
    
    if(statevector[2]==0){
      eventtimes=append(eventtimes,timenow)
      eventnames=append(eventnames,"ServStrt") 
    }
  }
  if(eventcal[1,2]=="ServStrt"){
    timenow=eventtimes[1]
    eventtimes=append(eventtimes,timenow+(-1/mu)*log(1-R))
    eventnames=append(eventnames,"ServCompl")
   
    duration = timenow - lastchangeTime
    statevector[3] = duration
    sv=rbind(sv,statevector)
    
    statevector[1]  = statevector[1]-1
    statevector[2]  = 1
    lastchangeTime = timenow
    
  }
  if(eventcal[1,2]=="ServCompl"){
    timenow=eventtimes[1]
    
    if(statevector[1]>0){
      eventtimes=append(eventtimes,timenow)
      eventnames=append(eventnames,"ServStrt")  
    }
    
    duration = timenow - lastchangeTime
    statevector[3] = duration
    sv=rbind(sv,statevector)
    
    statevector[2]  = 0
    lastchangeTime = timenow
  
  }

eventtimes=eventtimes[-1]
eventnames=eventnames[-1]
eventnames=eventnames[order(eventtimes)]
eventtimes=eventtimes[order(eventtimes)]
eventcal=data.frame(eventtimes,eventnames)
pass=pass+1
#print(pass)
#eventcal
}

eventcal=data.frame(eventtimes,eventnames)
eventcal
sv


y=sv[,1]
y=append(c(0),y)
x=sv[,3]
lastx=0
cumx=0
cx=c(0,0)
for(i in 2:length(x)-1){
  cumx=cumx+x[i]
  cx[i+1]=cumx
}
cx
  
length(y)  
length(cx)


plot(stepfun(cx,y))

ZZ=data.frame(x,cx,y[-1],sv[,2])
ZZ


numinserv=sum(sv[,2]*sv[,3])/sum(sv[,3])

numinserv

numinq=sum(sv[,1]*sv[,3])/sum(sv[,3])

numinq

(numinq+numinserv)/lambda

numinq/lambda