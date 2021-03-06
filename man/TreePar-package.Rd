\name{TreePar-package}
\alias{TreePar-package}
\alias{TreePar}
\docType{package}
\title{
Estimating birth and death rates based on phylogenies}
\description{
(i) For a given species phylogeny on present day data which is calibrated to calendar-time, a method for estimating maximum likelihood speciation and extinction processes is provided. The method allows for non-constant rates. Rates may change (1) as a function of time, i.e. rate shifts at specified times or mass extinction events (likelihood implemented as LikShifts, optimization as bd.shifts.optim and visualized as bd.shifts.plot) or (2) as a function of the number of species, i.e. density-dependence (likelihood implemented as LikDD and optimization as bd.densdep.optim) or (3) extinction rate may be a function of species age (likelihood implemented as LikAge and optimization as bd.age.optim.matlab). Note that the methods take into account the whole phylogeny, in particular it accounts for the "pull of the present" effect. (1-3) can take into account incomplete species sampling, as long as each species has the same probability of being sampled. For a given phylogeny on higher taxa (i.e. all but one species per taxa are missing), where the number of species is known within each higher taxa, speciation and extinction rates can be estimated under model (1) (implemented within LikShifts and bd.shifts.optim with groups !=0). (ii) For a given phylogeny with sequentially sampled tips, e.g. a virus phylogeny, rates can be estimated under a model where rates vary across time using bdsky.stt.optim based on likelihood LikShiftsSTT (extending LikShifts and bd.shifts.optim). Furthermore, rates may vary as a function of host types using LikTypesSTT (multitype branching process extending functions in R package diversitree). This function can furthermore calculate the likelihood under an epidemiological model where infected individuals are first exposed and then infectious.
}
\details{
\tabular{ll}{
Package: \tab TreePar\cr
Type: \tab Package\cr
Version: \tab 3.3\cr
Date: \tab 2015-01-02\cr
License: \tab GPL-2 \cr
LazyLoad: \tab yes\cr
}
}
\author{
Tanja Stadler
%Maintainer: Tanja Stadler
<http://www.bsse.ethz.ch/cEvo>
}
\references{
T. Stadler. Mammalian phylogeny reveals recent diversification rate shifts. Proc. Nat. Acad. Sci., 108(15): 6187-6192, 2011.
}
%\keyword{tree, phylogeny, diversification, mass extinction, maximum likelihood}
\seealso{
\code{ape}
\code{TreeSim}
}