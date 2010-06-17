treemrca <-
function(x,t,l,mu,rho) {
	#x<-sort(x)
	n<-lineages(x,t)
	mrca<-x[length(x)]
	res<- n[1]*log(rho[1])  + n[1]*2* log(l[1]-mu[1]) + 2 * log(g(mrca,t,l,mu,rho))-2*log(1-q2(inter(mrca,t),mrca,t,l,mu,rho  ) )
	for (j in 1:(length(x)-1)) {
		res <- res +log(l[inter(x[j],t)]) + log(g(x[j],t,l,mu,rho))
		}
	if (length(t)>1) {
		for (j in 2:length(t)) {
			res <- res + n[j] * log(rho[j] *(l[j]-mu[j])^2 * g((t[j]),t,l,mu,rho) )
		}
	}
	-res
	}

