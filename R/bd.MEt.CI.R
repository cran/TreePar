bd.MEt.CI <-
function (x,rho,pointest)  {
	x<-sort(x)
	cutoff<-1 #round((length(x)+1)/10)
    dev <- function(p) {
    	boundary<-1
    	l<-vector()
    	mu<-vector()
    	t<-vector()
    	help <- (length(p)+1)/3
    	for (i in 1:help) {
    		ltemp<-p[i+help]/(1-p[i])
    		mutemp<-p[i] * p[i+help]/(1-p[i])
			l<-c(l, ltemp)
    		mu<- c(mu,mutemp)  
   			if (i<help){	
   				ttemp<-p[i+2*help]
   				t<-c(t,ttemp)
   				if (ttemp<x[cutoff] || ttemp>x[(length(x) -cutoff+1)]){ boundary=0}
   			} 
   			if ((mutemp<0) || (mutemp >= ltemp) || (ltemp<=0) ) {
    			boundary=0
    		}    			
   		}
		if (boundary==1 ){
			t<-c(0,t)
   			t<-sort(t)
			out<-treemrca(x,t,l,mu,rho)}
        else {out<-10^100}
        #else {out<-NA}
        out
    }
	out <- pointest	
	###
	###
	#out$par<-c(out$par,3.3,7.9,33.1)
	#print(out)
	#for the mammals 3 shifts!
	###
	###
	obj<-bd.MEt.conf(out,dev)
	para<-partransformvector(c(out$par)) 
	#list(obj,para,out)
	obj
    }

