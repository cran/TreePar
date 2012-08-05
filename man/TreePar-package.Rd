\name{TreePar-package}
\alias{TreePar-package}
\alias{TreePar}
\docType{package}
\title{
Estimating birth and death rates based on phylogenies}
\description{
(i) For a given species phylogeny on present day data which is calibrated to calendar-time, a method for estimating maximum likelihood speciation and extinction processes is provided. The method allows for non-constant rates. Rates may change (1) as a function of time, i.e. rate shifts at specified times or mass extinction events (implemented as bd.shifts.optim and visualized as bd.shifts.plot) or (2) as a function of the number of species, i.e. density-dependence (implemented as bd.densdep.optim). Note that the method takes into account the whole phylogeny, in particular it accounts for the "pull of the present" effect. Both models can take into account incomplete species sampling, as long as each species has the same probability of being sampled. For a given phylogeny on higher taxa (i.e. all but one species per taxa are mission), but where the number of species is known within each higher taxa, speciation and extinction rates can be estimated under model (1) (implemented as bd.shifts.optim with groups !=0). 

(ii) For a given phylogeny with sequentially sampled tips, e.g. a virus phylogeny, rates can be estimated under a model where rates vary across time using bdsky.stt.optim (extending bd.shifts.optim). Furthermore rates may vary as a function of host types using bdtypes.stt.lik (multitype branching process extending functions in R package diversitree).
}
\details{
\tabular{ll}{
Package: \tab TreePar\cr
Type: \tab Package\cr
Version: \tab 2.3\cr
Date: \tab 2012-08-05\cr
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
T. Stadler: Mammalian phylogeny reveals recent diversification rate shifts. PNAS 108 (15), 6187-6192, 2011.

R.S. Etienne, B. Haegeman, T. Stadler, T. Aze, P.N. Pearson, A. Purvis and A.B. Phillimore: Diversity-dependence brings molecular phylogenies closer to agreement with the fossil record. Proc. Roy. Soc. B, doi: 10.1098/rspb.2011.1439, 2011.

T. Stadler, F. Bokma: Estimating speciation and extinction rates for phylogenies of higher taxa. Submitted, 2012.

A. Lambert & T. Stadler: Coalescent point processes and phylogenies. Manuscript.
}
%\keyword{tree, phylogeny, diversification, mass extinction, maximum likelihood}
\seealso{
\code{ape}
\code{TreeSim}
}