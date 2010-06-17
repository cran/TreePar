bd.MEt <-
function (x,rho)  {
	check<-vector()
    dev <- function(p) {
    	p[check]<-0
    	l<-vector()
    	mu<-vector()
    	t<-vector()
    	help <- (length(p)+1)/3
    	for (i in 1:help) {
			l<-c(l, p[i+help]/(1-p[i]))
    		mu<- c(mu,p[i] * p[i+help]/(1-p[i]))  
   			if (i<help){	
   				t<-c(t,p[i+2*help])} 
   			}
   			t<-c(0,t)
		treemrca(x,t,l,mu,rho)
    }
    numb<-length(rho)
    init<-vector()
    for (i in 1:numb){
    	init<-c(0.5,0.5,init,((i)/numb*x[length(x)]))
    	}
    init<- init[-length(init)]
    out <- nlm(function(p) dev(p[1:(3*numb-1)]), init, hessian = TRUE)
    check<-which(out$estimate < 0)
    out2<-0
    helpvar<-0
    if (length(check) > 0){
    	out2 <- nlm(function(p) dev(p[1:(3*numb-1)]), init, hessian = TRUE)
    	out2$estimate[check]=0
    	helpvar<-1
    	}    
	if (helpvar==0) {para<-partransformvector(c(out$estimate)) } else {para<-partransformvector(c(out2$estimate)) }    
	list(out,out2,para)
    }

