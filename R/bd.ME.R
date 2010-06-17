bd.ME <-
function (x,t,rho)  {
	check<-vector()
    dev <- function(p) {
    	p[check]<-0
    	l<-vector()
    	mu<-vector()
    	help <- length(p)/2
    	for (i in 1:help) {
			l<-c(l, p[i+help]/(1-p[i]))
    		mu<- c(mu,p[i] * p[i+help]/(1-p[i]))  		}
        treemrca(x,t,l,mu,rho)
    }
    numb<-length(rho)
    init<-vector()
    for (i in 1:numb){
    	init<-c(init,0.5,0.5)
    	}
    out <- nlm(function(p) dev(p[1:(2*numb)]), init, hessian = TRUE)
    check<-which(out$estimate < 0)
    out2<-0
    helpvar<-0
    if (length(check) > 0){
    	out2 <- nlm(function(p) dev(p[1:(2*numb)]), init, hessian = TRUE)
    	out2$estimate[check]=0
    	helpvar<-1
    	}    
    if (helpvar==0) {para<-partransformvector(c(out$estimate,t[-1])) } else {para<-partransformvector(c(out2$estimate,t[-1])) }
    list(out,out2,para)
    }

