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
bd.densdep.optim(x,minK,maxK,discrete=TRUE,continuous=FALSE,lambdainit=2,muinit=1,Kinit=0)
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
}
\value{
\item{res}{Maximum likelihood speciation and extinction rate and the saturation value K; the first value is the result when K being discrete (0 if discrete=FALSE) and the second value is the result when K being continuous (0 if continuous=FALSE).}
}
%\references{
%}
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