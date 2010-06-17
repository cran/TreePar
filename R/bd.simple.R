bd.simple <-
function (x,rho) 
{	N <- length(x)+1
    dev <- function(a, r) {    
    	treemrcabd(x,(r/(1-a)),(a*r/(1-a)),rho)	
       # -( (N - 2) * log(r) + r * sum(x[3:N]) +  N * log(1 - a) - 2 * sum(log(exp(r * x[2:N]) - a)))
    }
    out <- nlm(function(p) dev(p[1], p[2]), c(0.5, 0.5), hessian = TRUE)
    out2<-0
    helpvar<-0
    if (out$estimate[1] < 0) {
        out2 <- nlm(function(p) dev(0, p), 0.5, hessian = TRUE)
        out2$estimate<- c(0,out2$estimate)
        helpvar<-1
        }
	if (helpvar==0) {para<-partransformvector(c(out$estimate)) } else {para<-partransformvector(c(out2$estimate)) }
	list(out,out2,para)
}

