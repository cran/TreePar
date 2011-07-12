bd.densdep.optim<-function(x,minK=0,maxK=0,discrete=TRUE,continuous=FALSE,lambdainit=2,muinit=1,Kinit=0){
	x<-sort(x)
	if (minK==0) {minK<-length(x)+1}
	if (maxK==0) {maxK<-round(minK*1.5)}
	resdiscrete<-0
	rescont<-0
	if (discrete ==TRUE){
	resdiscrete<-bd.densdep.optim.discrete(x,maxK,minK)}
	if (continuous==TRUE){
		if (Kinit==0) {Kinit<-(length(x)+2)}
	rescont<-subplex(c(lambdainit,muinit,Kinit),LikComp2,model=-1,x=x,minN=minK,control=list(reltol=10^(-10)))}
res<-list(resdiscrete,rescont)
res
}
