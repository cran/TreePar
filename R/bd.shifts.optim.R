bd.shifts.optim <-
function(x,sampling,grid,start,end,yule=FALSE,ME=FALSE,maxitk=5){
	print("startest")
	shifts<-length(sampling)-1
	cuts<-round((end-start)/grid)

	miniall<-list()
	estall<-list()

	est0<-bd.ME.optim(x,c(0),c(sampling[1]),yule)
	miniall<-c(miniall,list(c(est0[[1]]$value,est0[[1]]$par)))
	estall<-c(estall,list(est0))

	timeshifts<-c(0)

	convfail<-vector()

	for (k in 1:shifts){
		est<-list()
		lik<-vector()
		timevec<-vector()
		mini<-c(200000)
		miniindex<-0
		for (j in 0:(cuts)){
			time1<-start + j/cuts*(end-start)
			timevec<-c(timevec,time1)
			timetemp<-sort(c(timeshifts,time1))
			if (ME==FALSE){
				temp<-bd.ME.optim(x,timetemp,sampling[1:length(timetemp)],yule,maxitk)
			} else {
				temp<-bd.ME.optim.rho(x,timetemp,sampling[1],yule,maxitk)
			}
			if (temp[[1]]$convergence != 0){
				print("convergence problem")
				convfail<-rbind(convfail,c(k,time1))
				}
			lik<-c(lik,temp[[1]]$value)
			if (temp[[1]]$value<mini[1] && length(which(time1==timeshifts))==0){
				mini<-c(temp[[1]]$value,temp[[1]]$par,timetemp[2:length(timetemp)])
				miniindex<-(j+1)
			}
			#print(mini)
			print(c(k,time1))
			est<-c(est,list(temp))	
		}
	estall<-c(estall,list(est))
	miniall<-c(miniall,list(mini))
	print(mini)
	timeshifts<-c(timeshifts,mini[length(mini)])
	}
	#print(miniindex)
	out<-list(estall,miniall,timevec,estall[[shifts+1]][[miniindex]],convfail)
	out
}

