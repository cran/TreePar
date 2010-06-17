\name{bd.ME.CI}
\alias{bd.ME.CI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
bd.ME.CI: Calculates the confidence intervals for point estimates obtained from bd.ME.optim
}
\description{
bd.ME.CI calculates the confidence intervals for point estimates obtained from bd.ME.optim for fixed t.
}
\usage{
bd.ME.CI(x, t, rho, pointest)
}
\arguments{
  \item{x}{Vector of speciation times in the phylogeny. Time is measured increasing going into the past.}
  \item{t}{Vector of times when rates may change / mass extinction may occur. t is ordered increasing in time with the first entry being 0. Let the number of entries in t be m, with m arbitrary.}
  \item{rho}{Vector of length m. rho_i is the probability of a species surviving the mass extinction at time t_i. rho_1 is the probability of an extant species being sampled. rho_1=1 means that the considered phylogeny is complete. rho_i=1 means that at t_i, a rate shift may occur but no species go extinct.}
 \item{pointest}{Point estimate of the maximum likelihood method: bd.ME.optim(x,t,rho)[[1]]}
}
\value{
Matrix of maximum likelihood diversification rates (speciation rate - extinction rate) and turnover (extinction rate / speciation rate) with 95 per cent quantiles.
}
\references{
T. Stadler: Mammalian phylogeny reveals recent diversification rate shifts. Submitted. 2010.
}
\author{
Tanja Stadler 
}
\seealso{
bd.ME.optim, bd.MEt.optim, bd.MEt.CI
}
\examples{
## see TreePar main page
}
\keyword{diversification rates}
\keyword{rate shifts}
\keyword{estimating rates}
\keyword{phylogeny}