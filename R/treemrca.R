treemrca <-
function(x,t,l,mu,rho,posdiv=FALSE) {
	res<- -10^12
	boundary<-0
	for (i in 1:length(l)){
		if (l[i]==mu[i]|| mu[i]<0 || l[i]<0.0001 || l[i]>100 || (abs(l[i]-mu[i])<0.0001) ){boundary<-1}
		if (posdiv==TRUE && (l[i]-mu[i])<0.0001 ) {boundary<-1}
		}
	for (i in 1:length(rho)){
		if (rho[i]>1 || rho[i]<=0 ){boundary<-1}
		}
	if (boundary==0) {
	x<-sort(x)
	n<-lineages(x,t)
	mrca<-x[length(x)]
	res<- n[1]*log(rho[1])  + n[1]* log((l[1]-mu[1])^2) + 2 * log(g(mrca,t,l,mu,rho))-2*log(1-q2(inter(mrca,t),mrca,t,l,mu,rho  ) )
	for (j in 1:(length(x)-1)) {
		res <- res +log(l[inter(x[j],t)]) + log(g(x[j],t,l,mu,rho))
		}
	if (inter(mrca,t)>1) {
		for (j in 2:inter(mrca,t)) {
			res <- res + n[j] * log(rho[j] *(l[j]-mu[j])^2 * g((t[j]),t,l,mu,rho) )
		}
	}
	}
	-res
	}

