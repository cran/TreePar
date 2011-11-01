\name{bd.groups.optim}
\alias{bd.groups.optim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
bd.groups.optim: Estimating speciation and extinction rates in higher-taxonomic-level phylogenies}
\description{
bd.groups.optim estimates the maximum likelihood speciation and extinction rates under a constant rate speciation and extinction model based on a phylogeny on higher taxa, where the number of species within each taxa are known.
}
\usage{
bd.groups.optim(phy,S,xcut=0,lambda=0,mu=0)
}
\arguments{
  \item{phy}{
Phylogenetic tree on higher taxa.
}
  \item{S}{
Vector of number of species for each higher taxa.}
  \item{xcut}{Each species extant at cutoff time xcut in the past defines a higher taxa (implying that xcut must be after the most recent branching time in phi). xcut=0 is used if no xcut is specified and means that higher taxa are not defined using a cutoff, but higher taxa are randomly selected clades in the species phylogeny.
}
\item{lambda}{lambda=0 default. If lambda>0, then the likelihood for the given lambda and mu is calculated instead of finding the optimal parameter.}
\item{mu}{see lambda}}
\value{
\item{res}{IF lambda>0: return value is - log likelihood for the specified lambda and mu. ELSE result is maximum likelihood estimate:}
\item{res[[1]]}{First row: Maximum likelihood estimate for turnover (mu/lambda) with 95 per cent confidence intervals. Second row: Maximum likelihood estimate for diversification rate (lambda-mu) with 95 per cent confidence intervals.}
\item{res[[2]]$value}{- Log Likelihood value}
\item{res[[2]]$convergence}{Convergence information. 0 is successful. For other output see man for subplex.}
}
\note{
The likelihood is calculated assuming there were two lineages at the time of the root. The likelihood is NOT conditioned on survival of the two lineages. Likelihood-values from bd.shifts.optim are directly comparable (eg AIC). Likelihood-values from laser are comparable after the transformation -res+(sum(log(2:length(x)))-(length(x)-1)*log(2)).
}
\references{
T. Stadler, F. Bokma: Estimating speciation and extinction rates for phylogenies of higher taxa. Submitted, 2011.
}
\author{
Tanja Stadler 
}
\examples{
### An example from Paradis (2003) using the avian orders:
data(bird.orders)
### Number of species in each order from Sibley and Monroe (1990):
S <- c(10, 47, 69, 214, 161, 17, 355, 51, 56, 10, 39, 152, 6, 143, 358, 103, 319, 23, 291, 313, 196, 1027, 5712)
xcut<-95*0.207407
out<-bd.groups.optim(bird.orders, S, xcut)
### Not accounting for the cutoff when defining the bird taxa estimates 0 extinction:
out1<-bd.groups.optim(bird.orders, S, 0)
### xcut=0 produces almost the same results as the following ape function
bd.ext(bird.orders, S)
}
%\keyword{diversification rates}
%\keyword{rate shifts}
%\keyword{mass extinction}
%\keyword{maximum likelihood rates}
%\keyword{phylogeny}