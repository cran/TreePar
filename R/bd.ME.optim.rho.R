bd.ME.optim.rho <-
function (x,t,sampling,yule = FALSE,maxitk=5)  {
	if (yule==TRUE) {
		print("not implemented. ext>0 !")
		#res<-bd.MEyule.optim(x,t,rho)
		} else {
	x<-sort(x)
    dev <- function(p) {
    	boundary=1
    	l<-vector()
    	mu<-vector()
    	help <- (length(p)+1)/3
    	for (i in 1:help) {
    		ltemp<-p[i+help]/(1-p[i])
    		mutemp<-p[i] * p[i+help]/(1-p[i])
			l<-c(l, ltemp)
    		mu<- c(mu,mutemp)
    		if ((mutemp<0) || (mutemp >= ltemp) || (ltemp<=0)) {
    			boundary=0
    			} 
    	}
    	rho<-c(sampling,p[(2*help+1):length(p)])
    	if (max(rho)>1 || min(rho)<0.01) {boundary<-0}
    	if (boundary==1 ){
        	out<- treemrca(x,t,l,mu,rho)}
        else {out<-10^12}
        out
    }
    
    numb<-length(t)
    init<-vector()
    for (i in 1:numb){
    	init<-c(0.001,init,0.05)
    	#init<-c(0.9,init,0.001)
    	}
    init <- c(init,(2:numb*0+0.7))
    out <- optim(init, dev,control=list(maxit=10000))#,method="SANN" )
    k<-1
    while (out$convergence != 0 && k<maxitk){ 
    		out <- optim(init, dev,control=list(maxit=10000*10^k)) 
    	 	k<-k+1
    	 }
    #para<-rbind(partransformvector(c(out$par[1:2*(length(out$par)+1)/3],t[-1])),c(sampling,out$par[(2*(length(out$par)+1)/3+1): length(out$par)]))
	para<-1
	res<-list(out,para)
	}
	res
    }

