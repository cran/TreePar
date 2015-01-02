\name{LikShifts}
\alias{LikShifts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
LikShifts: Calculates the likelihood of time-dependent birth and death rates given a phylogenetic tree.}
\description{
LikShifts calculates the likelihood of speciation and extinction rates and shift times for a given phylogenetic tree, conditioning on the age of the tree.
}
\usage{
LikShifts(x, t, lambda, mu, sampling, posdiv=FALSE,survival=1,groups=0)
}
\arguments{
  \item{x}{
Vector of speciation times in the phylogeny. Time is measured increasing going into the past with the present being time 0. x can be obtained from a phylogenetic tree using getx(TREE).
}
\item{t}{Vector of length m specifying the time of rate shifts (t[1]=0 is required, being the present). An entry in t may not coincide with an entry in x.}
\item{lambda,mu}{Vectors of the same length as t. l[i] (resp. mu[i]) specifies the speciation (resp. extinction rate) prior to t[i].}
  \item{sampling}{
Vector of same length as t. sampling[i] is the probability of a species surviving the mass extinction at time t[i]. sampling[1] is the probability of an extant species being sampled. sampling[1]=1 means that the considered phylogeny is complete. sampling[i]=1 (i>1) means that at time t[i], a rate shift may occur but no species go extinct.
}
  \item{posdiv}{
Not relevant when using LikShifts without optimizing (for bd.shifts.optim: posdiv=FALSE (default) allows the (speciation-extinction) rate to be negative, i.e. allows for periods of declining diversity. posdiv=TRUE forces the (speciation-extinction) rate to be positive).
}
\item{survival}{If survival = 1, the likelihood is conditioned on survival of the process (recommended). Otherwise survival = 0.}
\item{groups}{If groups != 0: the first column of groups indicates the age of higher taxa and the second column the number of species in the higher taxa (each row in groups corresponds to a leaf in the tree).}
}
\value{
\item{res}{-log likelihood of the model parameters for the given phylogenetic tree. The likelihood is calculated assuming there were two lineages at the time of the root. The likelihood is conditioned on survival of the two lineages if survival = 1. Likelihood values from bd.densdep.optim are directly comparable (eg. using AIC) for survival = 0. Likelihood values from laser are directly comparable to the TreePar output for survival = 0 after the TreePar output $value is transformed to -$value+sum(log(2:length(x))).}
}
\references{
T. Stadler. Mammalian phylogeny reveals recent diversification rate shifts. Proc. Nat. Acad. Sci., 108(15): 6187-6192, 2011.
}
\author{
Tanja Stadler 
}
%\keyword{diversification rates}
%\keyword{rate shifts}
%\keyword{mass extinction}
%\keyword{maximum likelihood rates}
%\keyword{phylogeny}
\examples{

timevec<-c(0,0.15,0.25)
lambdavec<-c(2.5,2,3)
muvec<-c(0.5,0.7,0.6)
x<-c(0.3,0.19,0.1)
x1<-c(x,max(x)*1.1)
x2<-c(x,max(x))
sampling<-0.4
grouptime<- rep(min(x)*0.95,length(x)+1)
group<- cbind(grouptime,grouptime*0+1)
group2 <- group
group2[1,2] <- 4
group2[2,2] <- 5
group2[3,2] <- 3
group3<-group
group3[2,2]<-10

### calculate likelihoods with root = 1

## Shifts in speciation / extinction rates (Stadler, PNAS 2011; Smrckova & Stadler, Manuscript 2014)
for (survival in c(0,1)) {
print(LikShiftsPP(x,timevec,lambdavec,muvec,sampling,survival=survival))
print(LikShifts(x,timevec,lambdavec,muvec,c(sampling,1,1),survival= survival))
print(LikShifts(x,timevec,lambdavec,muvec,c(sampling,1,1),survival= survival,groups=group))
print(LikShiftsSTT(par=c(lambdavec,muvec,timevec[-1]),x,x*0+1,sprob=c(0,0,0),
sampling=c(sampling,0,0),survival=survival,root=1))
print(" ")
}

## Shifts in speciation / extinction rates with group sampling
for (survival in c(0,1)) {
print(LikShifts(x,timevec,lambdavec,muvec,c(sampling,1,1),survival= survival,groups=group2))
print(LikShifts(x,timevec,lambdavec,muvec,c(sampling,1,1),survival= survival,groups=group3))
print(" ")
}

## Constant speciation and extinction rates 
# condition on age of tree x[1] and number of tips n
LikShiftsPP(x,timevec[1],lambdavec[1],muvec[1],sampling,n=1)
LikConstantn(lambdavec[1],muvec[1],sampling,x)
print(" ")
# condition on age of tree x[1]
for (survival in c(0,1)) {
print(LikConstant(lambdavec[1],muvec[1],sampling,x,root=1,survival=survival))
print(LikShiftsSTT(par=c(lambdavec[1],lambdavec[1],muvec[1],muvec[1],1),x,x*0+1,
sprob=c(0,0),sampling=c(sampling,1),survival=survival,root=1))
print(LikShiftsPP(x,c(0),lambdavec[1],muvec[1],sampling,root=1,survival=survival))
print(LikShifts(x,c(0),lambdavec[1],muvec[1],c(sampling),survival=survival))
print(LikShifts(x,c(0),lambdavec[1],muvec[1],c(sampling),survival= survival,groups=group))
print(LikShifts(x,c(0),c(lambdavec[1],lambdavec[1],lambdavec[1]),
c(muvec[1],muvec[1],muvec[1]),c(sampling,1,1),survival= survival))
\donttest{ if (survival == 0 ) {
	print(LikDD(c(lambdavec[1],muvec[1], 200), 
	model=0 ,root=1, x=sort(x),sampling=sampling)[1])  }
if (survival == 0 ) {
	print(LikDD(c(lambdavec[1],muvec[1], 300), 
	model=-1 ,root=1, x=sort(x),sampling=sampling)[1])  }}
print(" ")
}

## Diversity-dependent speciation rates
# condition on age of tree x[1], survival = 0
N<-10
pars <- matrix(c(N,lambdavec[1],muvec[1],0,sampling),nrow=1)
\donttest{
print(LikDD(c(lambdavec[1],muvec[1],N),x=sort(x),model=-1,root=1,sampling=sampling)[1])
## If you have the R package expoTree, you can compare:
# library(expoTree)
# print(-runExpoTree(pars,sort(x2),rep(1,length(x2)),survival=0)+(length(x)-1)*log(2))
print(" ")}

### calculate likelihoods with root = 0

## Constant speciation and extinction rates 
# condition on age of tree x[1] and number of tips n
print(LikShiftsPP(x,timevec[1],lambdavec[1],muvec[1],sampling,root=0,n=1))
print(LikConstantn(lambdavec[1],muvec[1],sampling,x,root=0))
print(" ")
# condition on age of tree x[1]
for (survival in c(0,1)){
print(LikShiftsPP(x,c(0),lambdavec[1],muvec[1],sampling,root=0,survival=survival))
print(LikConstant(lambdavec[1],muvec[1],sampling,x,root=0,survival=survival))
\donttest{if (survival == 0 ) {print(LikDD(c(lambdavec[1],muvec[1], 200), 
	model=0 ,root=0, x=sort(x),sampling=sampling)[1])  }
if (survival == 0 ) {print(LikDD(c(lambdavec[1],muvec[1], 300), 
	model=-1 ,root=0, x=sort(x),sampling=sampling)[1])  }}
print(" ")
}
## Diversity-dependent speciation rates
# condition on age of tree x[1], survival = 0
print(LikDD(c(lambdavec[1],muvec[1],N),x=sort(x),model=-1,root=0,sampling=sampling)[1])
## If you have the R package expoTree, you can compare:
#	print(-runExpoTree(pars,sort(x),rep(1,length(x)),survival=0)+(length(x)-1)*log(2))
}

