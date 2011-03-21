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
bd.densdep.optim(x,maxK)
}
\arguments{
  \item{x}{
Vector of speciation times in the phylogeny. Time is measured increasing going into the past with the present being time 0.
}
  \item{maxK}{
Maximum number of species which may occur (parameter K in model formulation above).
}
}
\value{
\item{res}{Maximum likelihood speciation and extinction rate and the saturation value}
}
\references{
#T. Stadler: Mammalian phylogeny reveals recent diversification rate shifts. In press, PNAS, 2011.
}
\author{
Tanja Stadler 
}
\examples{
#set.seed(1)
}
%\keyword{diversification rates}
%\keyword{rate shifts}
%\keyword{mass extinction}
%\keyword{maximum likelihood rates}
%\keyword{phylogeny}