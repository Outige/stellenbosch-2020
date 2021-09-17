source("/home/tieg/stellenbosch-2020/or322/a2/practice/testutils.R")
lambda=1/11

mu = 1/10
get_sv_21 = function() {
    sv = data.frame(0,0,0,0,0)
    names(sv)=c("queueL","server1B","server2B","stateDuration","stateStrtTime")

    stateVector = c(0,0,0,0,0)

    eventtimes=c(0)
    eventnames=c("arrival")

    eventcal = data.frame(eventtimes,eventnames)
    #evencal

    pass = 0

    setruntime=1000

    m=2^31 -1
    a=7^5
    c=100100100
    x=987654321

    lambda=1/11

    mu = 1/10

    s=2

    timenow=0

    lastchangeTime = 0

    while(timenow < setruntime) {
    x = (a*x + c)%%m
    r=x/m
    
    if(eventcal[1,2]=="arrival"){
        timenow=eventtimes[1]
        eventtimes=append(eventtimes,timenow+(-1/lambda)*log(1-r))
        eventnames=append(eventnames,"arrival")
        
        duration = timenow-lastchangeTime
        
        stateVector[4]=duration
        stateVector[5]=lastchangeTime
        sv=rbind(sv,stateVector)
        
        stateVector[1]=stateVector[1]+1
        lastchangeTime=timenow
        
        if(stateVector[2]==0){
        eventtimes=append(eventtimes,timenow)
        eventnames=append(eventnames,"Serv1Strt")
        }else if (stateVector[3]==0){
        eventtimes=append(eventtimes,timenow)
        eventnames=append(eventnames,"Serv2Strt")
        }
    }

    if(eventcal[1,2]=="Serv1Strt"){
        timenow=eventtimes[1]
        eventtimes=append(eventtimes,timenow+(-1/mu)*log(1-r))
        eventnames=append(eventnames,"Serv1Compl")
        
        duration = timenow - lastchangeTime
        stateVector[4] = duration
        stateVector[5] = lastchangeTime
        sv=rbind(sv,stateVector)
        
        stateVector[1]=stateVector[1]-1
        stateVector[2]=1
        
        lastchangeTime = timenow
    }
    
    if(eventcal[1,2]=="Serv2Strt"){
        timenow=eventtimes[1]
        eventtimes=append(eventtimes,timenow+(-1/mu)*log(1-r))
        eventnames=append(eventnames,"Serv2Compl")
        
        duration = timenow - lastchangeTime
        stateVector[4] = duration
        stateVector[5] = lastchangeTime
        sv=rbind(sv,stateVector)
        
        stateVector[1]=stateVector[1]-1
        stateVector[3]=1
        
        lastchangeTime = timenow
    }
    if(eventcal[1,2]=="Serv1Compl"){
        
        timenow=eventtimes[1]
        
        if(stateVector[1]>0){
        eventtimes=append(eventtimes,timenow)
        eventnames=append(eventnames,"Serv1Strt")
        }
        
        duration = timenow - lastchangeTime
        stateVector[4] = duration
        stateVector[5] = lastchangeTime
        sv=rbind(sv,stateVector)
        
        stateVector[2]=0
        lastchangeTime = timenow
        
    }
    
    if(eventcal[1,2]=="Serv2Compl"){
        
        timenow=eventtimes[1]
        
        if(stateVector[1]>0){
        eventtimes=append(eventtimes,timenow)
        eventnames=append(eventnames,"Serv2Strt")
        }
        
        duration = timenow - lastchangeTime
        stateVector[4] = duration
        stateVector[5] = lastchangeTime
        sv=rbind(sv,stateVector)
        
        stateVector[3]=0
        lastchangeTime = timenow
        
    }
    
    eventtimes = eventtimes[-1]
    eventnames=eventnames[-1]
    eventnames= eventnames[order(eventtimes)]
    eventtimes=eventtimes[order(eventtimes)]
    eventcal=data.frame(eventtimes,eventnames)
    #print(pass)
    #eventcal
    }

    eventcal = data.frame(eventtimes,eventnames)
    return(sv)
}

test_sim21 = function(sv, Ls1, Ls2, Lq) {
    sv_ = get_sv_21()
    Ls1_ = sum(sv_[,2]*sv_[,4])/sum(sv_[,4])
    Ls2_ = sum(sv_[,3]*sv_[,4])/sum(sv_[,4])
    Lq_ = sum(sv_[,1]*sv_[,4])/sum(sv_[,4])
    stopifnot(test_equal(sv, sv_))
    stopifnot(Ls1 == Ls1_)
    stopifnot(Ls2 == Ls2_)
    stopifnot(Lq == Lq_)
}
# eventcal
# sv