\name{bd.MEt.optim}
\alias{bd.MEt.optim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
bd.MEt.optim: Estimates the maximum likelihood diversification rates and rate shift times
}
\description{
bd.MEt.optim estimates the maximum likelihood speciation and extinction rates together with the rate shift times t in a (possibly incomplete sampled) phylogeny. At the times t, the rates are allowed to change and the species may undergo a mass extinction event. 
}
\usage{
bd.MEt.optim(x,  rho, init)
}
\arguments{
  \item{x}{Vector of speciation times in the phylogeny. Time is measured increasing going into the past.}
  \item{rho}{Vector of length m. rho_i is the probability of a species surviving the mass extinction at time t_i. rho_1 is the probability of an extant species being sampled. rho_1=1 means that the considered phylogeny is complete. rho_i=1 means that at t_i, a rate shift may occur but no species go extinct.}
  \item{init}{Vector of initial values ((mu_1 / l_1),..., (mu_m / l_m), (l_1 - mu_1),..., (l_m - mu_m), t_2, t_3,.., t_m) where mu_i is extinction rate and l_i is speciation rate in time interval [t_i,t_(i+1)]  }
}
\value{
\item{est[[1]]}{Matrix of maximum likelihood diversification (speciation-extinction) rates, turnover (speciation/extinction) and times of rate shifts together with the 95 per cent quantiles.}
\item{est[[3]]$value}{ - log(likelihood) value.}
}
\references{
T. Stadler: Mammalian phylogeny reveals recent diversification rate shifts. Submitted. 2010.
}
\author{
Tanja Stadler 
}
\seealso{
bd.ME.optim, bd.ME.CI, bd.MEt.CI
}
\examples{
## see TreePar main page
}
\keyword{diversification rates}
\keyword{rate shifts}
\keyword{estimating rates}
\keyword{phylogeny}
