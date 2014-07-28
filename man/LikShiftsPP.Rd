\name{LikShiftsPP}
\alias{LikShiftsPP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
LikShiftsPP: Calculates the likelihood of time-dependent birth and death rates given a phylogenetic tree.}
\description{
LikShiftsPP calculates the likelihood of speciation and extinction rates and shift times given a phylogenetic tree, conditioning on the age of the tree. This function uses the point process theory (Lambert and Stadler, 2013).
}
\usage{
LikShiftsPP(x, t, lambda, mu, sampling, survival=1,root=1,n=0)
}
\arguments{
  \item{x}{
Vector of speciation times in the phylogeny. Time is measured increasing going into the past with the present being time 0. x can be obtained from a phylogenetic tree using getx(TREE).
}
\item{t}{The time of rate shifts (t[1]=0 is required, being the present). An entry in t may not coincide with an entry in x.}
\item{lambda,mu}{Vectors of the same length as t. l[i] (resp. mu[i]) specifies the speciation (resp. extinction rate) prior to t[i].}
  \item{sampling}{
Sampling is the probability of an extant species being sampled and included into the tree.
}
\item{survival}{If survival = 1: The likelihood is conditioned on survival of the process (recommended). Otherwise survival = 0.}
\item{root}{If root = 1 then max(x) is the mrca (crown age), if root = 0 then max(x) is the age of a branch ancestral to the mrca (stem age).}
\item{n}{If n != 0 the likelihood is conditioned on n extant sampled tips and tree age max(x).}
}
\value{
\item{res}{-log likelihood of the model parameters given the phylogenetic tree.}
}
\references{
T. Stadler. Mammalian phylogeny reveals recent diversification rate shifts. Proc. Nat. Acad. Sci., 108(15): 6187-6192, 2011.

A. Lambert, T. Stadler. Macro-evolutionary models and coalescent point processes: the shape and probability of reconstructed phylogenies. Theo. Pop. Biol., 90: 113-128, 2013.}
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