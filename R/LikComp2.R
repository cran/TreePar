LikComp2 <- function(par,model,x,N){
	x<-sort(x)
	lamb0<-par[1]
	mu<-par[2]
	if (lamb0<=0 || mu<0 || lamb0==mu) {p<- NA}else {
	ntree=length(x)
	x<-c(0,x)
	p0=1:N*0
	p0[ntree]=1
	for (m in 1:(ntree)){
    	M=getmatrix(m,ntree,N,lamb0,mu,model)
    	M = M*(x[m+1]-x[m])
    	E= expm(M)
    	#E=MatrixExp(M,n=3)
    	pcurrent <- E%*%p0
    	pcurrent <- c(pcurrent[1:length(pcurrent)],0)
    	if (m <= ntree){
        	p0 <-pcurrent[(2:(N+1))]
        	for (k in 1:length(p0)){
        		p0[k]<-p0[k]*lambFun(k,N,lamb0,model)
        		}
        	}
    }
	p <- pcurrent[1]  /lambFun(1,N,lamb0,model)
	}
	p <- - p
	p
}