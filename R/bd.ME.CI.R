bd.ME.CI <-
function (x,t,rho,pointest)  {
	x<-sort(x)
    dev <- function(p) {
    	boundary<-1
    	l<-vector()
    	mu<-vector()
    	help <- (length(p))/2
    	for (i in 1:help) {
    		ltemp<-p[i+help]/(1-p[i])
    		mutemp<-p[i] * p[i+help]/(1-p[i])
			l<-c(l, ltemp)
    		mu<- c(mu,mutemp)  
   			if ((mutemp<0) || (mutemp >= ltemp) || (ltemp<=0) ) {
    			boundary=0
    		}    			
   		}
		if (boundary==1 ){
			out<-treemrca(x,t,l,mu,rho)}
        else {out<-10^100}
        #else {out<-NA}
        out
    }
	out <- pointest	
	obj<-bd.ME.conf(out,dev)
	obj
    }

