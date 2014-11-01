\name{create.mat}
\alias{create.mat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
create.mat: Generates input for LikAge and bd.age.optim.matlab.}
\description{
create.mat generates input for LikAge and bd.age.optim.matlab.
}
\usage{
create.mat(x, numgridpts=500, path, matfilename="setup")
}
\arguments{
  \item{x}{
Vector of speciation times in the phylogeny. Time is measured increasing going into the past with the present being time 0. x can be obtained from a phylogenetic tree using getx(TREE).
}	
  \item{numgridpts}{Number of gridpoints to be used in the numerical likelihood calculation in LikAge or bd.age.optim.matlab.}
  \item{path}{Path where your MCR (Matlab Compiler Runtime) is installed (for details see ?LikAge).}
\item{matfilename}{File matfilename.mat is created which contains input (speciation times) in a valid format for LikAge and bd.age.optim.matlab.}
  }
\value{
\item{res}{0 is returned. The file for downstream analysis is saved as matfilename.mat.
}
}
\note{
This function relies on compiled Matlab applications that need to be downloaded. For setting up the environment to run the function, please refer to 'Notes' in function 'LikAge'. This function is useful to evaluate if LikAge or bd.age.optim.matlab are called several times for one tree. Then the input for the matlab functions is created once and used in LikAge or bd.age.optim.matlab through inputformat=1 and specifying matfilename.}
\references{
H. Alexander, A. Lambert, T. Stadler. Quantifying Age-Dependent Extinction from Species Phylogenies. Submitted.}
\author{
Helen Alexander, Tanja Stadler}
\examples{
\donttest{	
# directory where MCR is installed
path<-"/Applications/MATLAB_R2014a_node.app/"
#location of folder TreePar_Matlab containing compiled Matlab applications (see ?LikAge)
locationMat<-"/Users/tstadler/Documents/Data/Uni/Research/R/TreeParProject/TreePar_Matlab"
#
x<-1:20
setwd(paste(locationMat,"/compiled",sep=""))
out<-create.mat(x,path=path)
}
}