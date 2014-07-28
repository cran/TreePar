\name{addroot}
\alias{addroot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
addroot: Adds a root edge ancestral to the first branching event.}
\description{
addroot adds a root edge ancestral to the first branching event.
}
\usage{
addroot(tree,rootlength)
}
\arguments{
  \item{tree}{
Tree of class phylo.
}
\item{rootlength}{Length of the root edge to be added.}}
\value{
\item{res}{Tree of class phylo with root edge added to $edge and $edge.length (alternative is to store root edge in $root.edge as done in ape).}
}

\author{
Tanja Stadler 
}
\examples{
set.seed(1)
# Simulate a tree
tree<-sim.bd.taxa(20,1,2,1,complete=FALSE,stochsampling=TRUE)
# add the root edge to the vector tree$edge and tree$edge.length
addroot(tree[[1]],tree[[1]]$root.edge)
}