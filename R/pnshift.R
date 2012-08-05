pnshift<-function(n,time,t,lambda,mu){
	i <- inter(time,t)
	rho<-lambda*0+1
	probext<-q2(i,time,t,lambda,mu,rho)
	res<-1-probext
	Finv<- 1/Ffuncshift(time,t,lambda,mu)
	res<-res*Finv*(1-Finv)^(n-1)
	res
}