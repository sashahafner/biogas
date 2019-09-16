\name{sludgeTwoSetup}
\alias{sludgeTwoSetup}
\docType{data}
\title{
  Setup Details for Batch Reactors
}
\description{
  Description of reactor substrates and information on bottle, substrates, inoculum initial masses, and headspace volume for 18 anaerobic bottles with primary wastewater sludge for substrate.
}
\usage{data("sludgeTwoSetup")}
\format{
  A data frame with 18 observations on the following 5 variables:
  \describe{
    \item{\code{id}}{identification code, a unique value for each bottle in the dataset. Numeric with all values within a range of 1-21.}
    \item{\code{descrip}}{treatment of the substrate (primary wastewater sludge), a character.}
    \item{\code{vol.hs}}{bottle headspace volume, in mL. A numeric vector.}
    \item{\code{m.inoc}}{mass of inoculum added to the bottle at the start in g, a numeric vector.}
    \item{\code{m.sub.vs}}{ mass of substrate added to the reactor at the start in g, a numeric vector.}
  }
}
\details{
  These data are meant to be example data for \code{\link{summBg}}.
  Substrate was primary wastewater sludge.
  
  Data in \code{sludgeTwoSetup} and \code{sludgeTwoBiogas} are from the same batch bottles.
}
\source{
  Sasha D. Hafner. Data were originally collected by Sergi Astals at the University of Queensland.
}
\examples{
data(sludgeTwoSetup)
}
\keyword{datasets}
\concept{biogas}
