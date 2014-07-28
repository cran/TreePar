BDSSnum.help<-function(phylo,rootedge,l,m,psi,summary,unknownStates,rtol,atol,migr) {
	newroot<-phylo$edge[rootedge,2]
	newtrees<-which(phylo$edge[,1]==newroot)
	tyoung<-summary[phylo$edge[rootedge,2]]
	told<-summary[phylo$edge[rootedge,1]]
	if (length(newtrees)==0) {
		if (unknownStates==FALSE && phylo$states[newroot]>0){
		state<-phylo$states[newroot]   #seems good (19.4.12) ?? 20.2.2012
		initpsi<-c(0,0)
		initpsi[state]<-psi[state]} else {
			initpsi<-c(psi[1],psi[2])    #Tanja 19.4.12
		}
		inity1<-integrator2(c(1,1),l,m,psi,c(0,tyoung),rtol,atol,migr)
		res<-integrator(init=c(inity1,initpsi),l,m,psi,c(tyoung,told),rtol,atol,migr)
	} else {
		likleft<-BDSSnum.help(phylo,newtrees[1],l,m,psi,summary,unknownStates,rtol,atol,migr)
		likright<-BDSSnum.help(phylo,newtrees[2],l,m,psi,summary,unknownStates,rtol,atol,migr)
		#res1<-c(likleft[1],likleft[3]*likright[3]*l[1]*2+ likleft[3]*likright[4]*l[2]+ likleft[4]*likright[3]*l[2])  #state1 above joining
		#res2<-c(likleft[2],likleft[4]*likright[4]*l[4]*2+ likleft[3]*likright[4]*l[3]+ likleft[4]*likright[3]*l[3])  #state2 above joining
		
		res1<-c(likleft[1],likleft[3]*likright[3]*l[1]+ likleft[3]*likright[4]*l[2]/2+ likleft[4]*likright[3]*l[2]/2)  #state1 above joining
		res2<-c(likleft[2],likleft[4]*likright[4]*l[4]+ likleft[3]*likright[4]*l[3]/2+ likleft[4]*likright[3]*l[3]/2)  #state2 above joining
	
	
		# # print("t")
		# # print(tyoung)
		# # print("g0")
		# # print(likleft)
		# # print("g1")
		# # print(likright)
		
		if (migr==1){
		res1<-c(likleft[1],likleft[3]*likright[3]*l[1])  #state1 above joining
		res2<-c(likleft[2],likleft[4]*likright[4]*l[4])  #state2 above joining			
		}
		
		res<-integrator(init=c(res1[1],res2[1],res1[2],res2[2]),l,m,psi,c(tyoung,told),rtol,atol,migr)
		# # print("res")
		# # print(res)
	}
	res
}
