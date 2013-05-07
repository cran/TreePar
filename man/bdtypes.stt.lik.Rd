\name{bdtypes.stt.lik}
\alias{bdtypes.stt.lik}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
bdtypes.stt.lik: calculates the likelihood of a tree under a 2-type birth-death model.}
\description{
bdtypes.stt.lik: calculates the likelihood of a tree under a 2-type birth-death model. For obtaining the maximum likelihood parameter estimates use the R function optim (see example below).
}
\usage{
bdtypes.stt.lik(par,phylo,fix=rbind(c(0,0),c(0,0)),sampfrac,
survival=0,posR=0,unknownStates=FALSE,root=0)
}
\arguments{
  \item{par}{
Parameters of the 2-type branching model in the order lambda11, lambda12, lambda21, lambda22, death1, death2, gamma12, gamma21. Currently gamma=0. Note that only a subset of these 8 parameters may be included in par, the remaining ones are in fix (using optim, the parameters not in fix will be estimated).
}
\item{phylo}{Phylogenetic tree for which the likelihood is calculated.}
\item{fix}{Determines which parameters are constraint when optimizing is performed. First row specifies the parameters being constraint (1 for lambda11, 2 for lambda12 etc). Second row: (i) If entry (2,j) is non-negative, say x, then parameter (1,j) is fixed to x. (ii) If entry (2,j) is negative, say -m, then parameter (1,j) is fixed to paramter m times entry (3,j) (exception is m=0.4: then the parameter lambda22 is fixed to lambda21*lambda12/lambda11, used in our paper for superspreaderdynamics).}
  \item{sampfrac}{
Vector of length 2. sampfrac[j] denotes the probability of sampling an individual in state j upon death (i.e. include the individual into the tree).
}
  \item{survival}{
survival=1 conditions the likelihood on sampling at least one tip (or one tip per root descendant if root=1). survival=0 default.
}
\item{posR}{posR=1 constrains the parameters (when optimizing) on R0>1. posR=0 default.}
\item{unknownStates}{If unknownStates=FALSE (default), phylo$states are used for the analysis. If unknownStates=TRUE, then the likelihood is calculated ignoring the tip states (used e.g. for identifying superspreader dynamics).}
\item{root}{root=0 indicates that there is an edge above the root (mrca) in the tree phylo. root=1 indicates that there is no edge above the root.}
}
\value{
\item{out}{-log likelihood of the tree given the parameters.}
}
\note{
This likelihood function extends the likelihood framework in the R package diversitree to trees with sequentially sampled tips.
}
\references{
T. Stadler, S. Bonhoeffer: Uncovering epidemiological dynamics in heterogeneous host populations using phylogenetic methods. Manuscript, 2012.
}
\author{
Tanja Stadler 
}
\examples{
set.seed(1)
lambda11<-15
lambda12<-3
lambda21<-1
lambda22<-3
death1<-4
death2<-4
sampprob1<-0.05
sampprob2<-0.05
l<-rbind(c(lambda11,lambda12),c(lambda21,lambda22))
d<-c(death1,death2)
s<-c(sampprob1,sampprob2)
n<-20
init<- -1     

tree<-sim.bdtypes.stt.taxa(n,l,d,s,init)

\donttest{
# Calculate likelihood for lambda11=15,lambda12=lambda21=lambda22=mu1=mu2=2,gamma=0
bdtypes.stt.lik(par=c(2,2,2,2),phylo=tree,
fix=rbind(c(1,6,7,8),c(15,-5,0,0),c(1,1,1,1)),sampfrac=s,survival=0,posR=0)
# Calculate maximum likelihood parameter estimates of lambda12,lambda21,
# lambda22,mu1 constraining lambda11=15,mu2=mu1 and gamma=0.
out<-try(optim(c(2,2,2,2),bdtypes.stt.lik,phylo=tree,fix=rbind(c(1,6,7,8),c(15,-5,0,0),c(1,1,1,1)),
sampfrac=s,survival=0,posR=0,control=list(maxit=10000)))
}}