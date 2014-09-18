\name{LikTypesSTT}
\alias{LikTypesSTT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
LikTypesSTT: Calculates the likelihood of the 2-type birth-death model parameters for a given tree.}
\description{
LikTypesSTT calculates the likelihood of the 2-type birth-death model parameters given a tree, conditioning on the age of the tree. For obtaining the maximum likelihood parameter estimates use the R function optim (see example below).
}
\usage{
LikTypesSTT(par,phylo,fix=rbind(c(0,0),c(0,0)),sampfrac,
survival=0,posR=0,unknownStates=FALSE,rtol=1e-12,atol=1e-12,migr=0,freq=0,cutoff=10^12)
}
\arguments{
  \item{par}{
Parameters of the 2-type branching model in the order lambda11, lambda12, lambda21, lambda22, death1, death2, gamma12, gamma21. Currently only gamma=0 is possible. Note that it is possible to only include a subset of these 8 parameters in par, the remaining ones are specified in fix (using optim, only the parameters specified in par will be estimated).
}
\item{phylo}{Phylogenetic tree for which the likelihood of the parameters is calculated.}
\item{fix}{Determines which parameters are constrained when optimizing is performed. First row of fix specifies the parameters being constrained (1 for lambda11, 2 for lambda12 etc). Second row of fix specifies the constrained parameters: (i) If entry [2,j] is non-negative, say x, then parameter [1,j] is fixed to x. (ii) If entry [2,j] is negative but not equal to -0.4, then parameter [1,j] is fixed to paramter -m times entry [3,j] (exception is m=-0.4: then the parameter lambda22 is fixed to lambda21*lambda12/lambda11, used in Stadler & Bonhoeffer (2013) full reference below).}
  \item{sampfrac}{
Vector of length 2. sampfrac[j] denotes the probability of sampling an individual in state j upon death (i.e. include the individual into the tree).
}
  \item{survival}{
survival=1 conditions the likelihood on sampling at least one tip. survival=0 default.
}
\item{posR}{posR=1 constrains the parameters (when optimizing) on the basic reproductive number R0 = >1. R0 for two types is calculated using TreePar:::R0types. posR=0 default.}
\item{unknownStates}{If unknownStates=FALSE (default), phylo$states are used for the analysis. If unknownStates=TRUE, then the likelihood is calculated ignoring the tip states (used e.g. for identifying superspreader dynamics).}
\item{rtol}{Relative tolerance parsed to the differential equation solver lsoda from package deSolve.}
\item{atol}{Absolute tolerance parsed to the differential equation solver lsoda from package deSolve.}
\item{migr}{If migr=0 then rate changes only at branching events (i.e. across-state transmission); if migr=1 then rate changes only along lineages (i.e. migration); if migr=2 then we assume a model with exposed state (1) and infectious state (2) where an infected individual transmits with rate lambda21 giving rise to a new exposed individual, and an exposed individual moves to the infectious class with rate lambda12.}
\item{freq}{Specifies the probability of the origin of the tree being of type 1. If freq=0 then the equilibrium frequency given by the parameters par is used (see Supplement in Stadler & Bonhoeffer (2013) for details).}
\item{cutoff}{We assume that prior to time cutoff, the sampling probability is 0. This setting acknowledges no sampling effort prior to a certain time. Cutoff must be older than the most ancestral tip. Default 10^12 means sampling throughout the whole time.}}
\value{
\item{out}{-log probability density of the (oriented) tree given the parameters (i.e. - log likelihood of the parameters for a fixed tree).}
}
\note{
This likelihood function extends the likelihood framework in the R package diversitree to trees with sequentially sampled tips. Our Ebola 2014 analyses highly relied on the function LikTypesSTT. Scripts used for this Ebola analysis are provided on our webpage www.bsse.ethz.ch/cevo. 
}
\references{
T. Stadler, S. Bonhoeffer. Uncovering epidemiological dynamics in heterogeneous host populations using phylogenetic methods. Phil. Trans. Roy. Soc. B, 368 (1614): 20120198, 2013.
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
tree<-addroot(tree,tree$root.edge)
\donttest{
# Calculate likelihood for lambda11=15,lambda12=lambda21=lambda22=mu1=mu2=2,gamma=0
LikTypesSTT(par=c(2,2,2,2),phylo=tree,
fix=rbind(c(1,6,7,8),c(15,-5,0,0),c(1,1,1,1)),sampfrac=s,survival=0,posR=0)
# Calculate maximum likelihood parameter estimates of lambda12,lambda21,
# lambda22,mu1 constraining lambda11=15,mu2=mu1 and gamma=0.
out<-try(optim(c(2,2,2,2),LikTypesSTT,phylo=tree,fix=rbind(c(1,6,7,8),c(15,-5,0,0),c(1,1,1,1)),
sampfrac=s,survival=0,posR=0,control=list(maxit=10000)))
}

### Likelihood calculation assuming a model with exposed class (migr = 2)
# Simulating a tree with exposed class
set.seed(2)
# simulate tree with expected incubation period of 14 days, 
# infectious period of 7 days, and R0 of 1.5:
mu <- c(0,1/7)
lambda <- rbind(c(0,1/14),c(1.5/7,0))
# sampling probability of infectious individuals is 0.35:
sampprob <-c(0,0.35)
# we stop once we have 20 samples:
n <- 20
# we simulate one tree:
numbsim<-1
# We mark first eliminate=10 tips such that we can easily drop them later
# (if deleting these 10 tips, we mimic no sampling close to the outbreak)
trees<-lapply(rep(n,numbsim),sim.bdtypes.stt.taxa,lambdavector=lambda,deathvector=mu,
sampprobvector=sampprob,EI=TRUE,eliminate=10)
tree<-trees[[1]]
origin<-max(getx(tree,sersampling=1)[,1])+tree$root.edge
# delete first eliminate=10 tips:
droptip<-tree$tip.label[which(tree$states == 1)]
phylo<-drop.tip(tree,droptip)
br<-getx(phylo,sersampling=1)
# we only sample after cutoff time:
cutoff<-max(br[which(br[,2]==0),1])*1.01
# add time since origin:
phylo<-addroot(phylo,origin-max(br))
# all tips were infectious:
phylo$states<-rep(2,length(phylo$tip.label))
##
# Evaluate likelihood at parameters used for simulation:
LikTypesSTT(c(0,lambda[1,2],lambda[2,1],0,0,mu[2],0,0),phylo,
sampfrac=sampprob,migr=2,cutoff=cutoff,freq=1)

#####################
#This little verifies the correctness of the implementation by permuting both states and rates

###################################################
test<-read.tree(text="((C:1.5,D:0.5):1,(A:1,B:1):3);")
test<-addroot(test,0.1)
par1<-2
par2<-1
par3<-0.5
par4<-3
par5<-1
par6<-0.5
par7<-1/3
par8<-0.5
###################################################

for (survival in c(0,1)) {
test$states<-c(2,1,2,1)
print(-LikTypesSTT(c(par4,par3,par2,par1,par6,par5,0,0),test,
sampfrac=c(par8,par7),survival=survival,rtol=10e-14,atol=10e-14,migr=0,freq=0.5))
test$states<-c(1,2,1,2)
print(-LikTypesSTT(c(par1,par2,par3,par4,par5,par6,0,0),test,
sampfrac=c(par7,par8),survival=survival,rtol=10e-14,atol=10e-14,migr=0,freq=0.5))
print(" ")
test$states<-c(2,1,2,1)
print(-LikTypesSTT(c(par4,par3,par2,par1,par6,par5,0,0),test,
sampfrac=c(par8,par7),survival=survival,rtol=10e-14,atol=10e-14,migr=1,freq=0.5))
test$states<-c(1,2,1,2)
print(-LikTypesSTT(c(par1,par2,par3,par4,par5,par6,0,0),test,
sampfrac=c(par7,par8),survival=survival,rtol=10e-14,atol=10e-14,migr=1,freq=0.5))
print(" ")
}
}
