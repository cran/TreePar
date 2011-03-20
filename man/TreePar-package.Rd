\name{TreePar-package}
\alias{TreePar-package}
\alias{TreePar}
\docType{package}
\title{
Estimating diversification rate changes and mass extinction events in phylogenies
}
\description{
For a given phylogeny on present day data which is calibrated to calendar-time, a method for estimating maximum likelihood speciation and extinction processes is provided. The method accounts for rate changes and mass extinction events: Time is split into intervals. Throughout an interval, the speciation and extinction rates stay constant. At the end of an interval, the speciation and extinction rates may change, and a fraction of the extant species may become extinct (mass extinction). No species becoming extinct at the end of an interval is a rate shift event without mass extinction. The method estimates the maximum likelihood time intervals, the speciation and extinction rates in each interval, and the fraction of species going extinct (unless the fraction is known and set to a predefined value). Note that the method takes into account the whole phylogeny, in particular it accounts for the "pull of the present" effect.
}
\details{
\tabular{ll}{
Package: \tab TreePar\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2011-03-20\cr
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
T. Stadler: Mammalian phylogeny reveals recent diversification rate shifts. In press, PNAS, 2011.
}
%\keyword{tree, phylogeny, diversification, mass extinction, maximum likelihood}
\seealso{
\code{ape}
\code{TreeSim}
}