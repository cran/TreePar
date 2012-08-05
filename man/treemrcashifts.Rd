\name{treemrcashifts}
\alias{treemrcashifts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
treemrcashifts: calculates the likelihood of a tree}
\description{
treemrcashifts: calculates the likelihood of a tree given the speciation and extinction rates and shift times.
}
\usage{
treemrcashifts(x, t, l, mu, sampling, posdiv=FALSE,survival=1,groups=0)
}
\arguments{
  \item{x}{
Vector of speciation times in the phylogeny. Time is measured increasing going into the past with the present being time 0. x can be obtained from a phylogenetic tree using getx(TREE).
}
\item{t}{The time of rate shifts (t[1]=0 is required, being the present)}
\item{l,mu}{Vectors of the same length as t. l[i] (resp. mu[i]) specifies the speciation (resp. extinction rate) prior to t[i].}
  \item{sampling}{
Vector of length m. sampling_i is the probability of a species surviving the mass extinction at time t_i. sampling_1 is the probability of an extant species being sampled. sampling_1=1 means that the considered phylogeny is complete. sampling_i=1 (i>1) means that at time t_i, a rate shift may occur but no species go extinct.
}
  \item{posdiv}{
Not relevant when using treemrcashifts without optimizing (for bd.shifts.optim: posdiv=FALSE (default) allows the speciation - extinction rate to be negative, i.e. allows for periods of declining diversity. posdiv=TRUE forces the speciation - extinction rate to be positive).
}
\item{survival}{If survival = 1: The likelihood is conditioned on survival of the process (recommended). Otherwise survival = 0.}
\item{groups}{If groups != 0: the first column of groups indicates the age of higher taxa and the second column the number of species in the higher taxa (each row in groups corresponds to a leaf in the tree).}
}
\value{
\item{res}{-log likelihood of the tree given the parameters. The likelihood is calculated assuming there were two lineages at the time of the root. The likelihood is conditioned on survival of the two lineages if survival = 1. Likelihood-values from bd.densdep.optim are directly comparable (eg AIC) for survival = 0. Likelihood-values from laser are comparable for survival = 0 and after the transformation -res+(sum(log(2:length(x)))-(length(x)-1)*log(2)).}
}
\references{
T. Stadler: Mammalian phylogeny reveals recent diversification rate shifts. PNAS 108 (15), 6187-6192, 2011.
}
\author{
Tanja Stadler 
}
%\keyword{diversification rates}
%\keyword{rate shifts}
%\keyword{mass extinction}
%\keyword{maximum likelihood rates}
%\keyword{phylogeny}