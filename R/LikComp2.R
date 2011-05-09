LikComp2 <- function(par,model,x,Ndec=-1,minN=0){
	if (minN==0){minN<-length(x)+1}
	x<-sort(x)
	lamb0<-par[1]
	mu<-par[2]
	if (Ndec<0){
		Ndec<-par[3]}
	if (lamb0<=0 || mu<0 || lamb0==mu || Ndec<minN) {p<- -10^10}else {
	ntree=length(x)
	x<-c(0,x)
	N<-ceiling(Ndec)
	p0=1:N*0
	p0[ntree]=1
	for (m in 1:(ntree)){
    	M=getmatrix(m,ntree,Ndec,lamb0,mu,model)
    	M = M*(x[m+1]-x[m])
    	E= expm(M)
    	#E=MatrixExp(M,n=3)
    	pcurrent <- E%*%p0
    	pcurrent <- c(pcurrent[1:length(pcurrent)],0)
    	if (m <= ntree){
        	p0 <-pcurrent[(2:(N+1))]
        	for (k in 1:length(p0)){
        		p0[k]<-p0[k]*lambFun(k,Ndec,lamb0,model)
        		}
        	}
    }
	p <- pcurrent[1]  /lambFun(1,Ndec,lamb0,model)
	}
	p <- - p
	p
}