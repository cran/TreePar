\name{bd.shifts.optim}
\alias{bd.shifts.optim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
bd.shifts.optim: Estimating speciation and extinction rate changes and mass extinction events in phylogenies.
}
\description{
bd.shifts.optim estimates the maximum likelihood speciation and extinction rates together with the rate shift times t=(t[1],t[2] .., t[m]) in a (possibly incomplete sampled) phylogeny. At the times t, the rates are allowed to change and the species may undergo a mass extinction event. 
}
\usage{
bd.shifts.optim(x, sampling, grid, start, end, maxitk = 5, yule = FALSE, ME = FALSE, 
all = FALSE, posdiv = FALSE, miniall = c(0), survival = 1,groups=0)
}
\arguments{
  \item{x}{
Vector of speciation times in the phylogeny. Time is measured increasing going into the past with the present being time 0. x can be obtained from a phylogenetic tree using getx(TREE).
}
  \item{sampling}{
Vector of length m. sampling[i] is the probability of a species surviving the mass extinction at time t[i]. sampling[1] is the probability of an extant species being sampled. sampling[1]=1 means that the considered phylogeny is complete. sampling[i]=1 (i>1) means that at time t[i], a rate shift may occur but no species go extinct. If ME=TRUE, all entries but sampling[1] will be discarded as they are estimated. However, you have to input a vector sampling of the appropriate length such that the program knows how many mass extinction events you want to allow for.
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
Only relevant when ME=TRUE. all=FALSE (default and recommended) estimates one speciation and one extinction rate for the whole tree, and estimates the intensities sampling[i] (i>1) of mass extinction events. all=TRUE allows for varying speciation and extinction rates. Since the parameters might correlate, all=TRUE is not recommended.
}
  \item{posdiv}{
posdiv=FALSE (default) allows the (speciation - extinction) rate to be negative, i.e. allows for periods of declining diversity. posdiv=TRUE forces the (speciation - extinction) rate to be positive.
}
  \item{miniall}{
If you have run the bd.shifts.optim for k shifts, but you now want to have K>k shifts, then set for the subsequent analysis the following: update sampling, and set miniall=res[[2]] where res[[2]] is the output from the run with k shifts.
}
\item{survival}{If survival = 1, the likelihood is conditioned on survival of the process (recommended). Otherwise survival = 0.}
\item{groups}{If groups != 0, the first column of groups indicates the age of higher taxa and the second column the number of species in the higher taxa (each row in groups corresponds to a leaf in the tree).}
}
\value{
\item{res[[1]][[i]]}{List of maximum likelihood parameter estimates for each fixed t (t determined by start, end, grid) where i-1 shifts are allowed to occur (i in 1:m). The first i entries are the turnover (extinction/speciation) estimates, for the successive intervals going back in time. The next i entries are the diversification rate estimates (speciation-extinction). The next i-1 entries are the probabilities that the lineage survives the mass extinction event (if ME=TRUE). (Note: if ME=TRUE and all=FALSE, the first entry is the turnover, the second the diversification rate, followed by the mass extinction survival probability).}
\item{res[[2]][[i]]}{Maximum likelihood parameter estimates for i-1 shifts (i in 1:m). First entry is the (-log likelihood) value. The next entries are the maximum likelihood parameter estimates (see res[[1]][[i]]). The last i-1 entries are the shift times. }
\item{res[[3]]}{Vector of time points where the function was evaluated.}
\item{res[[4]]}{Array specifying the time points when there was a convergence problem: a row of res[[4]] with entry (i,t[i]) means that when adding the i-th shift at time t[i], a convergence problem was encountered.}
}
\note{
The likelihood is calculated assuming there were two lineages at the time of the root. The likelihood is conditioned on survival of the two lineages if survival = 1. Likelihood values from bd.densdep.optim are directly comparable (eg. using AIC) for survival = 0. Likelihood values from package laser are comparable for survival = 0 after the TreePar output $value is transformed to -$value+sum(log(2:length(x))).
}
\references{
T. Stadler. Mammalian phylogeny reveals recent diversification rate shifts. Proc. Nat. Acad. Sci., 108(15): 6187-6192, 2011.

T. Stadler, F. Bokma. Estimating speciation and extinction rates for phylogenies of higher taxa. Syst. Biol., 62(2): 220-230, 2013. (for groups>0 but no rate shift).

A. Lambert, T. Stadler. Macro-evolutionary models and coalescent point processes: the shape and probability of reconstructed phylogenies. Theo. Pop. Biol., 90: 113-128, 2013. (for groups>0).
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
# Mass extinction intensities 0.5 at time 1 in past, 0.4 at time 2 in past. 
# Present day species are all sampled (rho[1]=1):
rho <- c(1,0.5,0.4)
# speciation rates (between t[i],t[i+1] we have speciation rate lambda[i]):
lambda <- c(2,2,1)
# extinction rates (between t[i],t[i+1] we have extinction rate mu[i]):
mu <- c(1,1,0)
# Simulation of a tree:
tree<-sim.rateshift.taxa(nspecies,1,lambda,mu,frac=rho,times=time,complete=FALSE)
# Extracting the speciation times x:
x<-sort(getx(tree[[1]]),decreasing=TRUE)

# When estimating the the rate shift times t based on branching times x, 
# we allow the shift times to be 0.6, 0.8, 1, 1.2, .. ,2.4:
start <- 0.6
end <- 2.4
grid <- 0.2

\donttest{
# We fix rho and estimate time, lambda, mu:
res <- bd.shifts.optim(x,rho,grid,start,end)[[2]]
res
# res[[2]] tells us about the maximum likelihood estimate given one rate shift:
# - log lik = 17.330862988.
# rate shift at time 2.2.
# turnover (extinction/speciation) = 0.186301549 more recent than 2.2,
#     and = 0.939681843 more ancestral than 2.2.
# net diversification (speciation-extinction) rate = 0.958947381 more recent than 2.2, 
#     and = 0.000100009 more ancestral than 2.2.

#test if i shifts explain the tree significantly better than i-1 shifts, here i=1:
i<-1
test<-pchisq(2*(res[[i]][1]-res[[i+1]][1]),3)
#if test>0.95 then i shifts is significantly better than i-1 shifts at a 5% error

# We fix rho=1 and mu=0 and then estimate time, lambda:
resyule <- bd.shifts.optim(x,rho,grid,start,end,yule=TRUE)
resyule[[2]]
# We estimate time, rho, lambda, mu:
resrho <- bd.shifts.optim(x,rho,grid,start,end,ME=TRUE)
resrho[[2]]
# Data analysis in Stadler & Bokma, 2012:
# Number of species in each order from Sibley and Monroe (1990)
data(bird.orders)
S <- c(10, 47, 69, 214, 161, 17, 355, 51, 56, 10, 39, 152, 6, 143, 
358, 103, 319, 23, 291, 313, 196, 1027, 5712)
groups<-get.groups(bird.orders,S,0)
groupscut<-get.groups(bird.orders,S,96.43)
x<-branching.times(bird.orders)
# transforming molecular timescale into calendar timescale
groups[,1]<-groups[,1]/0.207407
x<-x/0.207407 
bd.shifts.optim(x,sampling=c(1),survival=1,groups=groups)[[2]]
bd.shifts.optim(x,sampling=c(1),survival=1,groups=groupscut)[[2]]
# allowing one shift in rates:
bd.shifts.optim(x,sampling=c(1,1),grid=1,start=20,end=25,survival=1,groups=groupscut)[[2]]
}
}
%\keyword{diversification rates}
%\keyword{rate shifts}
%\keyword{mass extinction}
%\keyword{maximum likelihood rates}
%\keyword{phylogeny}