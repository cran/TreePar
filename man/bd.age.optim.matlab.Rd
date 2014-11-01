\name{bd.age.optim.matlab}
\alias{bd.age.optim.matlab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
bd.age.optim.matlab: Estimating speciation rate and age-dependent extinction rate in phylogenies.
}
\description{
bd.age.optim.matlab estimates the maximum likelihood speciation and extinction rates in a (possibly incomplete sampled) phylogeny. Speciation rate is constant, time to extinction is assumed to be a Gamma distribution.
}
\usage{
bd.age.optim.matlab(x, lambdainit=1, kinit=1, thetainit=2, sampling, model="G", root=1, 
inputformat=0,precision=4,numgridpts=500, path,matfilename="setup")
}
\arguments{
  \item{x}{
Vector of speciation times in the phylogeny. Time is measured increasing going into the past with the present being time 0. x can be obtained from a phylogenetic tree using getx(TREE).
}	
\item{lambdainit}{Speciation rate at the initial point from which optimization starts.}
	  \item{kinit}{
Shape parameter of the Gamma distribution modelling time to extinction at the initial point from which optimization starts.}
  \item{thetainit}{
Scale parameter of the Gamma distribution modelling time to extinction at the initial point from which optimization starts.}
  \item{sampling}{Probability to sample a present day lineage.}
  \item{model}{Model assumed for time to extinction. "G": Gamma distribution, i.e. shape parameter is estimated. "E": Exponential distribution, i.e. shape parameter is set to 1.}
\item{root}{If root=1, max(x) is the crown age and the likelihood is conditioned on the crown age. If root=0, max(x) is the stem age and the likelihood is conditioned on the stem age.}
\item{inputformat}{If 0, then speciation time vector 'x' is supplied. If 1, then mat-file input (obtained by create.mat) in 'matfilename' is supplied, and 'x' is not considered.}
  \item{precision}{Number of decimal places returned from the numerical likelihood calculation.}
  \item{numgridpts}{Number of gridpoints (evenly spaced time points between zero and the tree age, max(x)) used in the numerical likelihood calculation. The higher this number, the more accurate the numerical results will be.}
  \item{path}{Path where your MCR (Matlab Compiler Runtime) is installed (for details see ?LikAge).}
\item{matfilename}{Only relevant if inputformat=1; then matfilename="setup" if input file is setup.mat.}
  }
\value{
\item{res}{Vector of four numbers: negative log likelihood value and maximum likelihood estimate of lambda, k, theta.
}
}
\note{
This function relies on compiled Matlab applications that need to be downloaded. For setting up the environment to run the function, please refer to 'Notes' in function 'LikAge'.}
\references{
H. Alexander, A. Lambert, T. Stadler. Quantifying Age-Dependent Extinction from Species Phylogenies. Submitted.}
\author{
Helen Alexander, Tanja Stadler}
\examples{
\donttest{	
# directory where MCR is installed
path<-"/Applications/MATLAB_R2014a_node.app/"
#location of folder TreePar_Matlab
locationMat<-"/Users/tstadler/Documents/Data/Uni/Research/R/TreeParProject/TreePar_Matlab"
#
setwd(locationMat)
x<-read.table("branchingtimes.txt")
numgridpts<-500
lambda<-2
k<-1
theta<-1
sampling<-1
setwd(paste(locationMat,"/compiled",sep=""))
LikAge(x,lambda, k, theta, sampling, path=path)
outG <- bd.age.optim.matlab(x, lambda, k, theta, sampling, path=path)
outE <- bd.age.optim.matlab(x, lambda, k, theta, sampling, path=path, model="E")
# compare with bd.shifts.optim for exponenetial lifetime
outEcompare<-c(outE[1],1/(outE[2]*outE[3]),outE[2]-1/outE[3])
outShifts <- bd.shifts.optim(x,sampling=c(1),survival=1)[[2]]
}
}