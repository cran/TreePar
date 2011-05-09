\name{getx}
\alias{getx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getx: Getting the vector of speciation times x from a phylogenetic tree
}
\description{
getx calculates the input vector x for the rate estimation functions from a phylogenetic tree (which may have polytomies)
}
\usage{
getx(datatree)
}
\arguments{
  \item{datatree}{
phylogenetic tree
}
}
\value{
\item{x}{Vector of speciation times}
}
\references{
#T. Stadler: Mammalian phylogeny reveals recent diversification rate shifts. In press, PNAS, 2011.
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