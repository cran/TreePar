\name{bd.ME.optim}
\alias{bd.ME.optim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
bd.ME.optim: Estimates the maximum likelihood diversification rates which may change at given points in the past
}
\description{
bd.ME.optim estimates the maximum likelihood speciation and extinction rates in a (possibly incomplete sampled) phylogeny. At pre-defined times t (input), the rates are allowed to change and the species may undergo a mass extinction event. 
}
\usage{
bd.ME.optim(x, t, rho)
}
\arguments{
  \item{x}{Vector of speciation times in the phylogeny. Time is measured increasing going into the past.}
  \item{t}{Vector of times when rates may change / mass extinction may occur. t is ordered increasing in time with the first entry being 0. Let the number of entries in t be m, with m arbitrary.}
  \item{rho}{Vector of length m. rho_i is the probability of a species surviving the mass extinction at time t_i. rho_1 is the probability of an extant species being sampled. rho_1=1 means that the considered phylogeny is complete. rho_i=1 means that at t_i, a rate shift may occur but no species go extinct.}
}
\value{
\item{est[[1]]$par[i] }{For i<=m: estimated turnover (extinction rate / speciation rate) in interval [t_i,t_(i+1)] (with t_(m+1) = infinity). For i>m: estimated diversification rate (speciation rate - extinction rate) in interval [t_(i-m),t_(i-m+1)].}
\item{est[[1]]$value }{ - log(likelihood) value.}
\item{est[[2]] }{Matrix with estimates: First row speciation rate lambda, second row extinction rate mu. i-th entry in row is the rate in interval [t_(i),t_(i+1)]. Third row are the shift times (fixed to t).}
}
\references{
T. Stadler: Mammalian phylogeny reveals recent diversification rate shifts. Submitted. 2010.
}
\author{
Tanja Stadler 
}
\seealso{
bd.MEt.optim, bd.ME.CI, bd.MEt.CI
}
\examples{
## see TreePar main page
}
\keyword{diversification rates}
\keyword{rate shifts}
\keyword{estimating rates}
\keyword{phylogeny}
