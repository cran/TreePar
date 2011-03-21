bd.densdep.optim<-function(x,maxK){
minlik<-0
for (k in (length(x)+1):maxK){
	res<-subplex(c(2,1),LikComp2,model=-1,x=x,N=k)
	print(k)
	print(res)
	if (res$value<minlik){
		minlik<-res$value
		mini<-res
		miniN<-k
		}
	print(miniN)
	print(mini)
}
mini$par<-c(mini$par,miniN)
res<-mini
res
}