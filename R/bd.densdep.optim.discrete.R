bd.densdep.optim.discrete<-function(x,maxN,minN){
minlik<-0
for (k in minN:maxN){
	res<-subplex(c(2,1),LikComp2,model=-1,x=x,Ndec=k,minN=minN)
	print(k)
	print(res)
	if (res$value<minlik){
		minlik<-res$value
		mini<-res
		miniN<-k
		}
	#print(miniN)
	print(mini)
}
mini$par<-c(mini$par,miniN)
res<-mini
res
}
