\name{bd.densdep.optim}
\alias{bd.densdep.optim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
bd.densdep.optim: Estimating speciation and extinction rates in phylogenies under a density-dependent speciation model
}
\description{
bd.densdep.optim estimates the maximum likelihood speciation and extinction rates under a density-dependent speciation model. Speciation rate is a function of the number of species N, lambda(N) = max(0,lambda(1-N/K)), and extinction rate is mu (constant).
}
\usage{
bd.densdep.optim(x,minK,maxK,discrete=TRUE,continuous=FALSE,lambdainit=2,muinit=1,Kinit=0,Yule=FALSE,muset=0,rho=1,model=-1)
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
If continuous=TRUE, the likelihood function is maximized with K being a continuous parameter. The function subplex is used for optimization and sometimes gets stuck at a non-optimal K. Thus I always recommend to also calculate with discrete=TRUE.}
  \item{lambdainit}{Initial lambda value for optimization when K is continuous (default is 2).}
  \item{muinit}{Initial mu value for optimization when K is continuous (default is 1).}
  \item{Kinit}{Initial K value for optimization when K is continuous (default is (number of species)+1).}
  \item{Yule}{Yule=FALSE is default. Yule=TRUE sets mu=0, i.e. no extinction.}
  \item{muset}{muset=0 (default) maximizes over the whole parameter range. muset>0 means that the optimization is done over all mu>muset. muset<0 fixes mu=-muset.}
  \item{rho}{rho=1 is default meaning all species are sampled. rho<1 assumes that the phylogeny is incomplete, and each species is included with probability rho.}
  \item{model}{model=-1 (default) is the density-dependent model. model=0 (only relevant for testing purposes) assumes that lambda is constant for number of species < K, and 0 for number of species >= K. model=0 is used for testing / comparing to constant rate model implemented in bd.shifts.optim.}
}
\value{
\item{res}{Maximum likelihood speciation and extinction rate and the saturation value K; the first entry, res[[1]], is the result when K being discrete (0 if discrete=FALSE) and the second entry, res[[2]], is the result when K being continuous (0 if continuous=FALSE). $par is (lambda,mu,K). $value is -log likelihood. The likelihood is calculated assuming there were two lineages at the time of the root. The likelihood is NOT conditioned on survival of the two lineages. Likelihood-values from bd.shifts.optim are directly comparable (eg AIC). Likelihood-values from laser are comparable after the transformation -$value+(sum(log(2:length(x)))-(length(x)-1)*log(2)).}
}
\note{
bd.densdep.optim(x,Yule=TRUE,discrete=FALSE,cont=TRUE) in TreePar and DDL(x) in Laser return the same results (up to transforming the -log likelihood ($value) from TreePar via -$value+(sum(log(2:length(x)))-(length(x)-1)*log(2))
}
\references{
R.S. Etienne, B. Haegeman, T. Stadler, T. Aze, P.N. Pearson, A. Purvis and A.B. Phillimore: Diversity-dependence brings molecular phylogenies closer to agreement with the fossil record. Submitted to Proc. Roy. Soc. B.
}
\author{
Tanja Stadler 
}
%\examples{
%#set.seed(1)
%}
%\keyword{diversification rates}
%\keyword{rate shifts}
%\keyword{mass extinction}
%\keyword{maximum likelihood rates}
%\keyword{phylogeny}