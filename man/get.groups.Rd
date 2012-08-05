\name{get.groups}
\alias{get.groups}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get.groups generates input for bd.shifts.optim when groups!=0
}
\description{
get.groups generates input for bd.shifts.optim if the phylogeny is not resolved on the species level (groups!=0)
}
\usage{
get.groups(tree,S,xcut=0)
}
\arguments{
  \item{tree}{
phylogenetic tree to be analyzed with bd.shifts.optim}
  \item{S}{
S[i]: Number of species in tree represented by leaf i
}
  \item{xcut}{
Age of the higher taxa. If xcut=0: age of higher taxa is the length of the edge corresponding to the higher taxa. If xcut>0: age of each higher taxa is xcut}
}
\references{
T. Stadler, F. Bokma: Estimating speciation and extinction rates for phylogenies of higher taxa. Submitted, 2012.
}
\author{
Tanja Stadler 
}