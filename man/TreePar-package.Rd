\name{TreePar-package}
\alias{TreePar-package}
\alias{TreePar}
\docType{package}
\title{
Estimating diversification rate changes in phylogenies
}
\description{
For a given phylogeny on present day data (e.g. species or viruses), a likelihood method is provided where the maximum likelihood speciation and extinction rates are estimated. The method accounts for rate changes and mass extinction events: Time is split into intervals, and in each interval, the rates can be different. Throughout an interval, the rates stay constant. At the end of an interval, a pre-defined fraction of the extant species becomes extinct. No species becoming extinct is a rate shift event without mass extinction. The method estimates the maximum likelihood interval times, and the speciation and extinction rates in each interval.

Note that the method takes into account the whole phylogeny, in particular it accounts for the "pull of the present" effect.
}
\details{
\tabular{ll}{
Package: \tab TreePar\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2010-06-17\cr
License: \tab GPL-2 \cr
LazyLoad: \tab yes\cr
}
}
\author{
Tanja Stadler
%Maintainer: Tanja Stadler
 <http://www.tb.ethz.ch/people/tstadler>
}
\references{
T. Stadler: Mammalian phylogeny reveals recent diversification rate shifts. Submitted. 2010.
}
\keyword{diversification rates, rate shifts, estimating rates, phylogeny}
\seealso{
\code{ape}
\code{TreeSim}
}
\examples{
set.seed(1)

# number of leaves:
n<-20

# one tree:
numbsim<-1

# time shifts:
t<-c(0, 1,2)

# lambda_i is the speciation rate in [t_i,t_{i+1}] (t_{m+1} = infinity):
lambda<-c(0.5,4,4)

# mu_i is the extinction rate in [t_i,t_{i+1}] (t_{m+1} = infinity):
mu<-c(0,0.05,0.05)

# rho_i is the fraction of species going extinct at time t_i:
rho<-c(0.9,0.1,1)

# simulation of a tree:
trees1 <- sim.rateshift.taxa(n, numbsim, lambda, mu, rho ,t, complete = FALSE)

# extracting branching times from the tree:
x <- branching.times(trees1[[1]])

## Estimating the parameters lambda, mu for the simulated tree, fixing t:
est <- bd.ME.optim(x,t,rho)

# Calculating the 95 % confidence intervals for the parameter estimates:
conf <- bd.ME.CI(x,t,rho,est[[1]])

# If t shall be estimated, use bd.ME.optim for different t (fine grid). For the best t_max (e.g. t_max = (1,2) ), the confidence interval can be calculated as:
estopt<-est
estopt[[1]]$par<-c(est[[1]]$par,1,2)
conft <- bd.MEt.CI(x,rho,estopt[[1]])

# If t shall be estimated, you can also us bd.MEt.optim to simultaneously estimate t and the rates. Be careful as the method often gets stuck in local optima of t for big datasets (I prefer bd.ME.optim evaluated for varying t):

# starting value:
init<-c(0.1,0.1,0.1,0.6,4,4,1.3,1.9)
estt <- bd.MEt.optim(x,rho,init)
}
