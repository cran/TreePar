\name{LikShiftsSTT}
\alias{LikShiftsSTT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
LikShiftsSTT: Calculates likelihood of piecewise constant birth and death rates for a given phylogenetic tree with sequentially sampled tips.
}
\description{
LikShiftsSTT calculates likelihood of piecewise constant birth and death rates for a given phylogenetic tree with sequentially sampled tips, conditioning on the age of the tree.
}
\usage{
LikShiftsSTT(par,times,ttype,numbd=0,tconst=-1,sampling=0,
sprob,root=0,survival=1,tfixed=vector(),mint=0,maxt=0)
}
\arguments{
\item{par}{First k entries are piecewise constant speciation rates backward in time, second k entries are piecewise constant extinction rates backward in time, the last k-1 entries are the times of the rate shift.}
  \item{times}{
Vector of branching and sampling times in the phylogeny. Time is measured increasing going into the past with the present being time 0. times can be obtained from a phylogenetic tree using getx(TREE,sersampling=TRUE). An time of rate shift specified in par may not coincide with an entry in times.
}
\item{ttype}{If ttype[i]=0, then times[i] denotes a branching event. If ttype[i]=1, then times[i] denotes a sampling event.}
  \item{numbd}{Help variable when optimizing.}
  \item{tconst}{Help variable when optimizing.}
  \item{sampling}{Probability of sampling individuals at present. sampling=0 is default.
}
  \item{sprob}{
Vector of length k with the entries specifying the probability of a death event coinciding with sampling. In other words, sprob is the probability that an extinct node is sampled to appear in the observed phylogeny.}
  \item{root}{
root=0 indicates that there is an edge above the root (mrca) in the tree. root=1 indicates that there is no edge above the root.}
\item{survival}{If survival = 1, the likelihood is conditioned on survival of the process (recommended). Otherwise survival = 0.}
  \item{tfixed}{Help variable when optimizing.}
  \item{mint}{Help variable when optimizing.}
  \item{maxt}{Help variable when optimizing.}
}
\value{
\item{res}{-log likelihood of the parameters for the given tree.}}
\note{
LikShiftsSTT extends the function LikShifts to trees with sequentially sampled tips.
}
\references{ 
T. Stadler, D. Kuehnert, S. Bonhoeffer, A. Drummond. Birth-death skyline plot reveals temporal changes of epidemic spread in HIV and hepatitis C virus (HCV). Proc. Nat. Acad. Sci., 110(1): 228-233, 2013.
}
\author{
Tanja Stadler 
}
\examples{

###################################################
# Generating a tree
set.seed(1)
rootlength<-1
test0<-read.tree(text="((3:1.5,4:0.5):1,(1:2,2:1):3);")
test<-addroot(test0,rootlength)
test$Nnode<-4
test$states<-rep(1,4)
times<-getx(test,sersampling=1)[,1]
ttype<-getx(test,sersampling=1)[,2]
times0<-getx(test0,sersampling=1)[,1]
ttype0<-getx(test0,sersampling=1)[,2]
###################################################

# Likelihood calculation
# Tree with root edge
print(-LikShiftsSTT(c(2,1,0.8,0.5,1.5345),
times,ttype,sampling=0,sprob=c(1/3,2/3),survival=1,root=0)  )
# Tree without root edge
print(-LikShiftsSTT(c(2,1,0.8,0.5,1.5345),
times0,ttype0,sampling=0,sprob=c(1/3,2/3),survival=1,root=1)  )


###################################################
# This little example shows that in the case of constant rates, 
# LikShiftsSTT and LikTypesSTT yield the same results.
# In LikShiftsSTT root=0 or 1 allowed. In LikTypesSTT only root=0 possible.
death2<-c(3/2)
sampprob2<-c(1/3)
lambda2<-c(2)
t2<-c(0)
par2<-c(lambda2,death2)
#collapse to 1 state
epsi<- 0
test$states<-rep(1,4)
root<-0
for (survival in c(0,1)) {
print(-LikShiftsSTT(par2,times,ttype,sampling=0,sprob=sampprob2,survival=survival,root=root) )
print(-LikShiftsSTT(c(lambda2,lambda2,death2,death2,(max(times)*0.2431)),
times,ttype,sampling=0,sprob=c(sampprob2,sampprob2),survival=survival,root=root)  )
print(-LikShiftsSTT(par2,times,ttype,sampling=0,
sprob=c(sampprob2),root=root,survival=survival) )
print(-LikTypesSTT(c(lambda2,epsi,epsi,epsi,death2,epsi,0,0),test,
sampfrac=c(sampprob2,0),survival=survival,rtol=10e-10,atol=10e-10,freq=1,migr=0))
print(-LikTypesSTT(c(lambda2,epsi,epsi,epsi,death2,epsi,0,0),test,
sampfrac=c(sampprob2,0),survival=survival,rtol=10e-10,atol=10e-10,freq=1,migr=1)) 
print(" ")
}
}
%\keyword{diversification rates}
%\keyword{rate shifts}
%\keyword{mass extinction}
%\keyword{maximum likelihood rates}
%\keyword{phylogeny}