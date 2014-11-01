\name{LikAge}
\alias{LikAge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
LikAge: Calculates the likelihood of speciation and extinction rates for an ultrametric phylogeny under an age-dependent extinction model conditioning on the age of the tree. 
}
\description{
LikAge calculates the likelihood of speciation and extinction rates for an ultrametric phylogeny under an age-dependent extinction model conditioning on the age of the tree. Speciation rate is constant. Time to extinction is a Gamma distribution.
}
\usage{
LikAge(x, lambda, k, theta, sampling, root=1, inputformat=0,
precision=4, numgridpts=500, path, matfilename="setup")
}
\arguments{
  \item{x}{
Vector of speciation times in the phylogeny. Time is measured increasing going into the past with the present being time 0. x can be obtained from a phylogenetic tree using getx(TREE).
}	
\item{lambda}{Speciation rate.}
	  \item{k}{
Shape parameter of the Gamma distribution modelling time to extinction.}
  \item{theta}{
Scale parameter of the Gamma distribution modelling time to extinction.}
  \item{sampling}{Probability to sample a present day lineage.}
\item{root}{If root=1, max(x) is the crown age and the likelihood is conditioned on the crown age. If root=0, max(x) is the stem age and the likelihood is conditioned on the stem age.}
\item{inputformat}{If 0, then speciation time vector 'x' is supplied. If 1, then mat-file input (obtained by create.mat) in 'matfilename' is supplied, and 'x' is not considered.}
  \item{precision}{Number of decimal places returned from the numerical likelihood calculation.}
  \item{numgridpts}{Number of gridpoints (evenly spaced time points between zero and the tree age, max(x)) used in the numerical likelihood calculation. The higher this number, the more accurate the numerical results will be.}
  \item{path}{Path where your MCR (Matlab Compiler Runtime) is installed (for details see ?LikAge).}
\item{matfilename}{Only relevant if inputformat=1; then matfilename="setup" if input file is setup.mat.}
  }
\value{
\item{res}{negative log likelihood of the parameters given the tree. The likelihood conditions on sampling of at least 1 extant tip (analog to survival=1 in other likelihood functions of TreePar).
}
}
\note{
You do not need Matlab software to use the compiled Matlab applications. 
First you need to download the Matlab Compiler Runtime (MCR), freely available from Mathworks at: http://www.mathworks.com/products/compiler/mcr/.  You should download the same version with which the applications were compiled: if you use the compiled code we provide, this is v8.3.  Note the directory where you install MCR, it has to be inputted in LikAge as 'path'.
When you install MCR, the Installer gives the message: "On the target computer, append the following to you DYLD_LIBRARY_PATH environment variable:.." then it gives a path name which should be copied and pasted before closing the installer.
Second you have to download the folder TreePar_Matlab which is available on our github account: https://github.com/cevo-public. 
The compiled Matlab applications are in the subfolder TreePar_Matlab/compiled and should run on Mac OS version 10.7 and newer. 
If our compiled code does not run on your system, then you need to compile our Matlab code (m-files) in TreePar_Matlab/source to produce applications compatible with your system.  This requires a licensed copy of the Matlab software (see http://www.mathworks.com/). Open Matlab. Change to directory TreePar_Matlab/source. Then execute the following commands to compile the m-files into stand-alone applications using the Matlab compiler MCC:
>> mcc -m -R -nodisplay -R -nojvm EvalLFcn.m ;
>> mcc -m -R -nodisplay -R -nojvm MaxLFcn.m ;
>> mcc -m -R -nodisplay -R -nojvm ReadTreeFcn.m .
Copy the three resulting .sh and .app files to the folder TreePar_Matlab/compiled.
Note that if you have a licensed copy of Matlab and do not wish to use the R interface, the m-files we provide in TreePar_Matlab/source can also be used directly in Matlab for likelihood computations.
}
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
x<-1:20
numgridpts<-500
lambda<-2
k<-1
theta<-1
sampling<-1
# Provide path where the folder TreePar_Matlab is available
setwd(paste(locationMat,"/compiled",sep=""))
LikAge(x,lambda, k, theta, sampling, path=path)
# If the shape parameter is 1, we have a constant extinction rate, 
# corresponding to the function LikShift. Then the following commands return the same result.
LikAge(x,1, 1, 2, sampling, numgridpts=1000, path=path)
LikShifts(x,c(0),c(1),c(1/2),c(1))
}
}