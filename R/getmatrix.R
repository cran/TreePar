getmatrix<-function(m,ntree,N,lamb0,mu,model){
	if (m==1 && ntree==(N-1)){
		M<-spMatrix(N,N)
		M[N,N] =   - mu*N
		} else{
	#M<-matrix(0,N,N)
	M<-spMatrix(N,N)
	nm <- ntree-m+1
	for (k in (nm+1):(N-1)) {
    	M[k,k]= -(lambFun(k,N,lamb0,model)+mu)*k
    	M[k,k+1]=lambFun(k,N,lamb0,model) *(k+nm)
    	M[k,k-1] = mu * (k-nm)
	}
	M[nm,nm]= -(lambFun(nm,N,lamb0,model)+mu)*nm
	M[nm,(nm+1)]=lambFun(nm,N,lamb0,model)*(2*nm)
	M[N,N-1]= mu*(N-nm)
	M[N,N] =   - mu*N
	}
	M
	}