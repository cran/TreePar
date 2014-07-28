\name{LikDD}
\alias{LikDD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
LikDD: Calculates the likelihood of speciation and extinction rates for an ultrametric phylogeny under a density-dependent speciation model conditioning on the age of the tree. 
}
\description{
LikDD calculates the likelihood of speciation and extinction rates for a phylogeny under a density-dependent speciation model. Speciation rate is a function of the number of species N, lambda(N) = max(0,lambda(1-N/K)), where K is the carying capacity and lambda the speciation rate when N<<K. Extinction rate is mu (constant). For a computationally much faster implementation, please use function runExpoTree in R package expoTree. In contrast to LikDD, runExpoTree can handle trees with tips sampled sequentially through time.
}
\usage{
LikDD(par,model=-1,x,Ndec=-1,minN=0,psi=0,sampling=1,root=0,ki=0,muset=0,vec=0)
}
\arguments{
	\item{par}{Parameters (lambda,mu,K) for which to calculate the likelihood: lambda is speciation rate, mu is extinction rate, and K is carrying capacity.}
	  \item{model}{
model = -1: speciation rate is lambda(N) = max(0,lambda(1-N/K)). model = 0: speciation rate is lambda(N) = lambda for N <= K, und lambda(N) = 0 for N>K.}
  \item{x}{
Vector of speciation times and ancient lineage sampling times in the phylogeny. Time is measured increasing going into the past with the present being time 0. x can be obtained from a phylogenetic tree using getx(TREE).
}
  \item{Ndec}{
Help variable when optimizing.}
  \item{minN}{
Help variable when optimizing.}
  \item{psi}{
Rate for sampling lineages through time.}
  \item{sampling}{Probabiliy to sample a present day lineage.}
\item{root}{If root = 1 then max(x) is the mrca (crown age), if root = 0 then max(x) is the age of a branch ancestral to the mrca (stem age).}
  \item{ki}{Help variable when optimizing.}
  \item{muset}{Help variable when optimizing.}
  \item{vec}{vec=0 returns likelihood of model parameters for a tree given 1 lineage at beginning. vec=1 returns vector of likelihoods given i lineages at beginning.}
}
\value{
\item{res}{-log likelihood of the parameters given the tree.}
}
\note{
A faster version is now implemented in R package expoTree by Gabriel Leventhal. It is equivalent to the method here, see examples in LikShifts() manual. Further, expoTree can handle trees with sequentially sampled tips.
}
\references{
R.S. Etienne, B. Haegeman, T. Stadler, T. Aze, P.N. Pearson, A. Purvis, A.B. Phillimore. Diversity-dependence brings molecular phylogenies closer to agreement with the fossil record. Proc. Roy. Soc. B, 279: 1300-1309, 2012.

G. Leventhal, H. Guenthard, S. Bonhoeffer, T. Stadler. Using an epidemiological model for phylogenetic inference reveals density-dependence in HIV transmission. Mol. Biol. Evol., 31(1): 6-17, 2014.}
\author{
Tanja Stadler, 
Gabriel Leventhal
}
\examples{
# see manual of LikShifts()
}
%#set.seed(1)
%}
%\keyword{diversification rates}
%\keyword{rate shifts}
%\keyword{mass extinction}
%\keyword{maximum likelihood rates}
%\keyword{phylogeny}