\name{LikConstant}
\alias{LikConstant}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
LikConstant: Calculates the likelihood of constant birth and death rates for a given phylogenetic tree.}
\description{
LikConstant calculates the likelihood of constant speciation and extinction rates for a given phylogenetic tree, conditioning on the age of the tree.}
\usage{
LikConstant(lambda,mu,sampling,x,root=1,survival=1)
}
\arguments{
\item{lambda,mu}{Speciation and extinction rate.}
  \item{sampling}{
Sampling is the probability of an extant species being sampled and included into the tree.
}
  \item{x}{
Vector of speciation times in the phylogeny. Time is measured increasing going into the past with the present being time 0. x can be obtained from a phylogenetic tree using getx(TREE).
}
\item{root}{If root = 1 then max(x) is the mrca (crown age), if root = 0 then max(x) is the age of a branch ancestral to the mrca (stem age).}
\item{survival}{If survival = 1: The likelihood is conditioned on survival of the process (recommended). Otherwise survival = 0.}
}
\value{
\item{res}{-log likelihood of the birth and death rates given the tree.}
}
\references{
T. Stadler. On incomplete sampling under birth-death models and connections to the sampling-based coalescent. Jour. Theo. Biol. 261: 58-66, 2009.
}
\author{
Tanja Stadler 
}
\examples{
# see manual of LikShifts()
}

%\keyword{diversification rates}
%\keyword{rate shifts}
%\keyword{mass extinction}
%\keyword{maximum likelihood rates}
%\keyword{phylogeny}