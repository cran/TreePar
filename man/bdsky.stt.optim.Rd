\name{bdsky.stt.optim}
\alias{bdsky.stt.optim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
bdsky.stt.optim: Estimating piecewise constant birth and death rates in phylogenies with sequentially sampled tips.
}
\description{
bdsky.stt.optim estimates the maximum likelihood birth and death rates together with the rate shift times t=(t_1,t_2 .., t_m) in a phylogeny with sequentially sampled tips. At the times t, the rates are allowed to change. 
}
\usage{
bdsky.stt.optim(x,ttype=0,rho=0,sampprob=c(0),constdeath=0,root=0)
}
\arguments{
  \item{x}{
Vector of branching and sampling times in the phylogeny. Time is measured increasing going into the past with the present being time 0. x can be obtained from a phylogenetic tree using getx(TREE,sersampling=TRUE).
}
\item{ttype}{If ttype[i]=0, then x[i] denotes a branching event. If ttype[i]=1, then x[i] denotes a sampling event.}
  \item{rho}{Probability of sampling individuals at present. rho=0 is default.
}
  \item{sampprob}{
Vector of length k where k is the number of different birth rates to be estimated.}
   \item{constdeath}{
If constdeath=0 (default) then k death rates are estimated. If constdeath=1, then 1 death rate is estimated.
}
  \item{root}{
root=0 indicates that there is an edge above the root (mrca) in the tree. root=1 indicates that there is no edge above the root.}
}
\value{
\item{out[[1]]}{Entry [[j]] are the maximum likelihood parameter estimates for j-1 shifts}
\item{out[[2]]}{Matrix where in each row, first entry denotes the type of convergence problem, second entry denotes assuming the number of shifts in the problematic caluclation, third entry denotes in which interval it happened.}
}
\note{
bdsky.stt.optim extends the function bd.shifts.optim to trees with sequentially sampled tips.
}
\references{ T. Stadler, D. Kuehnert, S. Bonhoeffer, A. Drummond: The birth-death skyline plot. Manuscript, 2012.
}
\author{
Tanja Stadler 
}
\examples{
set.seed(1)

lambda<-c(3,4)
mu<-c(1,1)
sampprob<-c(0.5,0.5)
time<-c(0,0.5)
n<-10
	
tree<- sim.bdsky.stt(n,lambda,mu,time,sampprob)

summary<-getx(tree,sersampling=TRUE)
times<-summary[,1]
ttype<-summary[,2]
out <- bdsky.stt.optim(x=times,ttype=ttype,sampprob=sampprob)
}
%\keyword{diversification rates}
%\keyword{rate shifts}
%\keyword{mass extinction}
%\keyword{maximum likelihood rates}
%\keyword{phylogeny}