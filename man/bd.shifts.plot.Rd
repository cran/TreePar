\name{bd.shifts.plot}
\alias{bd.shifts.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
bd.shifts.plot plots the diversification rate estimates obtained with the function bd.shifts.optim
}
\description{
bd.shifts.plot plots the diversification rate estimates obtained with the function bd.shifts.optim
}
\usage{
bd.shifts.plot(resall,shifts,timemax=100,ratemin=-1,ratemax=1,plotturnover=FALSE)
}
\arguments{
  \item{resall}{
When k trees were analyzed, a list of length k with entries bd.shifts.optim[[2]] for each tree.
}
  \item{shifts}{
Specify under how many shifts you want to plot the estimated diversification rates (you can determine the number of significant shifts using a likelihood ratio test).
}
  \item{timemax}{
Specifies the upper end of the x-axis (time in past). Lower end is always 0.}
   \item{ratemin, ratemax}{
Specifies the upper and lower end of the y-axis (diversification rates).
}
  \item{plotturnover}{
Always diversification (speciation-extinction) is plotted. If plotturnover=TRUE, also turnover=speciation/extinction is plotted.
}
}
\references{
T. Stadler: Mammalian phylogeny reveals recent diversification rate shifts. PNAS 108 (15), 6187-6192, 2011.
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
x<-sort(getx(tree[[1]][[1]]),decreasing=TRUE)

# When estimating the shift times t for x, we allow the shift times to be 0.6, 0.8, 1, 1.2, .. ,2.4:
start <- 0.6
end <- 2.4
grid <- 0.2

# We fix rho and estimate time, lambda, mu:
res <- bd.shifts.optim(x,rho,grid,start,end)
res[[2]]
# We plot the result for 2 shifts:
bd.shifts.plot(list(res[[2]]),2,3,0,2)
}