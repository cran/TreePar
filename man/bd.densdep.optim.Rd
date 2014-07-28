\name{bd.densdep.optim}
\alias{bd.densdep.optim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
bd.densdep.optim: Estimating maximum likelihood speciation and extinction rates in phylogenies under a density-dependent speciation model. 
}
\description{
bd.densdep.optim estimates the maximum likelihood speciation and extinction rates under a density-dependent speciation model. Speciation rate is a function of the number of species N, lambda(N) = max(0,lambda(1-N/K)), where K is the carying capacity and lambda the speciation rate when N<<K. Extinction rate is mu (constant). For a computationally much faster implementation, please optimize the likelihood function with runExpoTree in R package expoTree. In contrast to bd.densdep.optim, runExpoTree can handle trees with tips sampled sequentially through time.
}
\usage{
bd.densdep.optim(x,minK,maxK,discrete=TRUE,continuous=FALSE,lambdainit=2,
muinit=1,Kinit=0,Yule=FALSE,muset=0,rho=1,model=-1)
}
\arguments{
  \item{x}{
Vector of speciation times in the phylogeny. Time is measured increasing going into the past with the present being time 0. x can be obtained from a phylogenetic tree using getx(TREE).
}
  \item{minK}{
Minimal value of K (when discrete=TRUE). Default is minK = (number of species).}
  \item{maxK}{
Maximal value of K (when discrete=TRUE). Default is maxK = 1.5(number of species).}
  \item{discrete}{
If discrete=TRUE, the likelihood function is maximized with K being an integer and the minimal size being minK and the maximal size being maxK.}
  \item{continuous}{
If continuous=TRUE, the likelihood function is maximized with K being a continuous parameter. The function subplex is used for optimization and sometimes gets stuck at a non-optimal K. Thus it is recommended to also calculate with discrete=TRUE.}
  \item{lambdainit}{Initial lambda value for optimization when K is continuous (default is 2).}
  \item{muinit}{Initial mu value for optimization when K is continuous (default is 1).}
  \item{Kinit}{Initial K value for optimization when K is continuous (default is Kinit=0 which automatically sets Kinit=(number of species)+1).}
  \item{Yule}{Yule=FALSE is default. Yule=TRUE fixes mu=0, i.e. no extinction.}
  \item{muset}{muset=0 (default) maximizes over the whole parameter range. muset>0 means that the optimization is done over all mu>muset. muset<0 fixes mu=-muset.}
  \item{rho}{rho=1 is default meaning all present-day species are sampled. 0<rho<1 assumes that the phylogeny is incomplete, and each present-day species is included with probability rho. rho=-1 means any number of present-day species N>=n has given rise to a sample of size n with probability 1. rho<-1 means that any number n,n+1,..,(-k) of present-day species may have given rise to a sample of size n with probability 1. rho>1 means that exactly rho>n present-day species gave rise to the sample n with probability 1. }
  \item{model}{model=-1 (default) is the density-dependent model. model=0 (only relevant for testing purposes) assumes that lambda is constant for number of species < K, and 0 for number of species >= K. model=0 is used for testing / comparing to constant rate model implemented in bd.shifts.optim.}
}
\value{
\item{res}{Maximum likelihood speciation rate lambda and extinction rate mu and the saturation value K; the first entry, res[[1]], is the result when K is discrete (0 if discrete=FALSE) and the second entry, res[[2]], is the result when K is continuous (0 if continuous=FALSE). $par is the maximum likelihood estimate of (lambda,mu,K). $value is the -log likelihood value. The likelihood is calculated assuming there were two lineages at the time of the root. The likelihood is NOT conditioned on survival of the two lineages. Likelihood values from bd.shifts.optim are directly comparable (eg. using AIC) for survival = 0. Likelihood values from laser are directly comparable to those obtained by bd.densdep.optim and bd.shifts.optim for survival = 0 after the TreePar output $value is transformed to -$value+sum(log(2:length(x))).}
}
\note{
A faster version is now implemented in R package expoTree by Gabriel Leventhal. It is equivalent to the method here, see examples for function LikDD. Further, it can handle trees with sequentially sampled tips.
bd.densdep.optim(x,Yule=TRUE,discrete=FALSE,continuous=TRUE) in TreePar and DDL(x) in Laser return the same results (up to transforming the -log likelihood ($value) from TreePar via -$value+sum(log(2:length(x))).
}
\references{
R.S. Etienne, B. Haegeman, T. Stadler, T. Aze, P.N. Pearson, A. Purvis, A.B. Phillimore. Diversity-dependence brings molecular phylogenies closer to agreement with the fossil record. Proc. Roy. Soc. B, 279: 1300-1309, 2012.

G. Leventhal, H. Guenthard, S. Bonhoeffer, T. Stadler. Using an epidemiological model for phylogenetic inference reveals density-dependence in HIV transmission. Mol. Biol. Evol., 31(1): 6-17, 2014.
}
\author{
Tanja Stadler, 
Gabriel Leventhal
}
\examples{
set.seed(1)
x<-c(10:1)
\donttest{
bd.densdep.optim(x,discrete=FALSE,continuous=TRUE)

# Laser returns same result for Yule model
res <- -bd.densdep.optim(x,Yule=TRUE,discrete=FALSE,continuous=TRUE)[[2]]$value 
res<-res+ sum(log(2:length(x)))
res
library(laser)
DDL(x)}
}
%\keyword{diversification rates}
%\keyword{rate shifts}
%\keyword{mass extinction}
%\keyword{maximum likelihood rates}
%\keyword{phylogeny}