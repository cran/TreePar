\name{bd.MEt.CI}
\alias{bd.MEt.CI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
bd.MEt.CI: Calculates the confidence intervals for point estimates obtained from bd.ME.optim for varying t
}
\description{
bd.MEt.CI calculates the confidence intervals for point estimates obtained from bd.ME.optim for varying t.
}
\usage{
bd.MEt.CI(x, rho, pointest)
}
\arguments{
  \item{x}{Vector of speciation times in the phylogeny. Time is measured increasing going into the past.
}
  \item{rho}{Vector of length m. rho_i is the probability of a species surviving the mass extinction at time t_i. rho_1 is the probability of an extant species being sampled. rho_1=1 means that the considered phylogeny is complete. rho_i=1 means that at t_i, a rate shift may occur but no species go extinct.}
 \item{pointest}{Point estimate of the maximum likelihood method: bd.ME.optim(x,t,rho)[[1]]. The component bd.ME.optim(x,t,rho)[[1]]$par has to be extended by the best shift times (obtained via using different shift times on a grid). See the examples.
}

}
\value{
Matrix of maximum likelihood diversification rates (speciation rate - extinction rate), turnover (extinction rate / speciation rate), and shift times with 95 per cent quantiles.
}
\references{
T. Stadler: Mammalian phylogeny reveals recent diversification rate shifts. Submitted. 2010.
}
\author{
Tanja Stadler 
}
\seealso{
bd.ME.optim, bd.MEt.optim, bd.ME.CI
}
\examples{
## see TreePar main page
}
\keyword{diversification rates}
\keyword{rate shifts}
\keyword{estimating rates}
\keyword{phylogeny}