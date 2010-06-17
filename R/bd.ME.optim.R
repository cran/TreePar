bd.ME.optim <-
function (x,t,rho)  {
	x<-sort(x)
    dev <- function(p) {
    	boundary=1
    	l<-vector()
    	mu<-vector()
    	help <- length(p)/2
    	for (i in 1:help) {
    		ltemp<-p[i+help]/(1-p[i])
    		mutemp<-p[i] * p[i+help]/(1-p[i])
			l<-c(l, ltemp)
    		mu<- c(mu,mutemp)
    		if ((mutemp<0) || (mutemp >= ltemp) || (ltemp<=0)) {
    			boundary=0
    			} 
    	}
    	if (boundary==1 ){
        	out<- treemrca(x,t,l,mu,rho)}
        else {out<-10^12}
        out
    }
    numb<-length(rho)
    init<-vector()
    for (i in 1:numb){
    	init<-c(0.001,init,0.05)
    	}
    	
    out <- optim(init, dev)#,method="SANN" )	
	para<-partransformvector(c(out$par,t[-1])) #t[2:length(t)])) 
	list(out,para)
    }

