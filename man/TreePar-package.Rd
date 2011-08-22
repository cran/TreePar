\name{TreePar-package}
\alias{TreePar-package}
\alias{TreePar}
\docType{package}
\title{
Estimating speciation and extinction rates based on phylogenies}
\description{
For a given species phylogeny on present day data which is calibrated to calendar-time, a method for estimating maximum likelihood speciation and extinction processes is provided. The method allows for non-constant rates. Rates may change (i) as a function of time, i.e. rate shifts at specified times or mass extinction events (implemented as bd.shifts.optim) or (ii) as a function of the number of species, i.e. density-dependence (implemented as bd.densdep.optim). Note that the method takes into account the whole phylogeny, in particular it accounts for the "pull of the present" effect. 

For a given phylogeny on higher taxa, but where the number of species is known within each higher taxa, speciation and extinction rates can be estimated, under the assumption that these rates remained constant (implemented as bd.groups.optim).
}
\details{
\tabular{ll}{
Package: \tab TreePar\cr
Type: \tab Package\cr
Version: \tab 2.1.2\cr
Date: \tab 2011-08-22\cr
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

T. Stadler: Estimating speciation and extinction rates under density-dependent models. Manuscript.

T. Stadler, F. Bokma: Estimating speciation and extinction rates for phylogenies of higher taxa. Submitted, 2011.
}
%\keyword{tree, phylogeny, diversification, mass extinction, maximum likelihood}
\seealso{
\code{ape}
\code{TreeSim}
}