\name{bd.shifts.optim}
\alias{bd.shifts.optim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
bd.shifts.optim: Estimating speciation and extinction rate changes and mass extinction events in phylogenies
}
\description{
bd.shifts.optim estimates the maximum likelihood speciation and extinction rates together with the rate shift times t=(t_1,t_2 .., t_m) in a (possibly incomplete sampled) phylogeny. At the times t, the rates are allowed to change and the species may undergo a mass extinction event. 
}
\usage{
bd.shifts.optim(x, sampling, grid, start, end, maxitk = 5, yule = FALSE, ME = FALSE, all = FALSE, posdiv = FALSE)
}
\arguments{
  \item{x}{
Vector of speciation times in the phylogeny. Time is measured increasing going into the past with the present being time 0.
}
  \item{sampling}{
Vector of length m. sampling_i is the probability of a species surviving the mass extinction at time t_i. sampling_1 is the probability of an extant species being sampled. sampling_1=1 means that the considered phylogeny is complete. sampling_i=1 (i>1) means that at time t_i, a rate shift may occur but no species go extinct. If ME=TRUE, all entries but sampling_1 will be discarded as they are estimated (however, input a vector sampling of the appropriate length such that the program knows how many mass extinction events you want to allow for).
}
  \item{grid, start, end}{
The model parameters are optimized for different fixed rate shift times. The fixed rate shift times are specified by being at (start, start+grid, start+2*grid .. end). I calculate the likelihood for the different rate shift times t instead of optimizing t with the function optim used for the other parameters, as the optimization performed poor for t (namely getting stuck in local optima).}
   \item{yule}{
yule=TRUE sets the extinction rates to zero.
}
  \item{maxitk}{
Integer value defining how many iterations shall be done in the optimization. Default is 5, but needs to be increased if too many warnings "convergence problem" appear.
}
  \item{ME}{
ME=FALSE (default) uses the mass extinction fractions specified in sampling and does not estimate them. If ME=FALSE is used with sampling=c(1,1, .. , 1), no mass extinction events are considered.
}
  \item{all}{
Only relevant when ME=TRUE. all=FALSE (default and recommended) estimates one speciation and one extinction rate for the whole tree, and estimates the intensities sampling_i (i>1) of mass extinction events. all=TRUE allows for varying speciation and extinction rates. Since the parameters might correlate, all=TRUE is not recommended.
}
  \item{posdiv}{
posdiv=FALSE (default) allows the speciation - extinction rate to be negative, i.e. allows for periods of declining diversity. posdiv=TRUE forces the speciation - extinction rate to be positive.
}
}
\value{
\item{res[[1]][[i]]}{List of maximum likelihood parameter estimates for each fixed t where i-1 shifts are allowed to occur (i in 1:m).}
\item{res[[2]][[i]]}{Maximum likelihood parameter estimates for i-1 shifts (i in 1:m): First entry is the (-log likelihood) value. The next i entries are the turnover (extinction/speciation) estimates, for the successive intervals going back in time. The next i entries are the diversification rate estimates (speciation-extinction). The next i-1 entries are the sampling estimates (if ME=TRUE). The last i-1 entries are the shift times. (Note: if ME=TRUE and all==FALSE, the second entry is the turnover, the third the diversification rate, followed by the sampling estimates).}
\item{res[[3]]}{Vector of time points where the function was evaluated.}
\item{res[[4]]}{Maximum likelihood estimates for m-1 shifts.}
\item{res[[5]]}{Array specifying the time points when there was a convergence problem: a row of res[[5]] with entry (i,t_i) means that when adding the i-th shift at time t_i, a convergence problem was encountered.}
}
\references{
T. Stadler: Mammalian phylogeny reveals recent diversification rate shifts. In press, PNAS, 2011.
}
\author{
Tanja Stadler 
}
\examples{
set.seed(1)

# First we simulate a tree, and then estimate the parameters for the tree:
# Number of species
nspecies <- 20
# At time 1 and 2 in the past, we have a rate shift:
time <- c(0,1,2)
# Mass extinction intensities 0.5 at time 1 in past, 0.4 at time 2 in past. Present day species are all sampled (rho_1=1):
rho <- c(1,0.5,0.4)
# speciation rates (between t_i,t_{i+1} we have speciation rate lambda_i):
lambda <- c(2,2,1)
# extinction rates (between t_i,t_{i+1} we have extinction rate mu_i):
mu <- c(1,1,0)
# Simulation of a tree:
tree<-sim.rateshift.taxa(nspecies,1,lambda,mu,frac=rho,times=time,complete=FALSE)
# Extracting the speciation times x:
x<-sort(branching.times(tree[[1]]),decreasing=TRUE)

# When estimating the shift times t for x, we allow the shift times to be 0.6, 0.8, 1, 1.2, .. ,2.4:
start <- 0.6
end <- 2.4
grid <- 0.2

# We estimate time, rho, lambda, mu:
resrho <- bd.shifts.optim(x,rho,grid,start,end,ME=TRUE)
resrho[[2]]
# We fix rho and estimate time, lambda, mu:
res <- bd.shifts.optim(x,rho,grid,start,end)
res[[2]]
# We fix rho=1 and mu=0 and then estimate time, lambda:
resyule <- bd.shifts.optim(x,rho,grid,start,end,yule=TRUE)
resyule[[2]]
}
%\keyword{diversification rates}
%\keyword{rate shifts}
%\keyword{mass extinction}
%\keyword{maximum likelihood rates}
%\keyword{phylogeny}