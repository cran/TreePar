bd.MEyule.optim <-
function (x,t,rho,maxitk=5)  {
	x<-sort(x)
    dev <- function(p) {
    	boundary=1
    	for (i in 1:length(p)) {
    		if (p[i]<=0) {
    			boundary=0
    		} 
    	}
    	if (boundary==1 ){
        	out<- treemrca(x,t,p,p*0,rho)}
        else {out<-10^12}
        out
    }
    numb<-length(rho)
    init<-vector()
    for (i in 1:numb){
    	init<-c(init,1)
    	#init<-c(0.9,init,0.001)
    	}
    if (length(init)==1){
	    out <- optimize(dev,interval=c(0,10))	
	    out$par<-out$minimum
	    out$value<-out$objective	

	} else {
	    	out <- optim(init, dev,control=list(maxit=10000))#,method="SANN"
	        k<-1
    		while (out$convergence != 0 && k<maxitk){ 
    			out <- optim(init, dev,control=list(maxit=10000*10^k)) 
    			k<-k+1
    	 	}
	}
	#para<-rbind(out$par,t*0,t)
	#rownames(para)<-c("l","mu","t")
	para<-1
	list(out,para)
    }

