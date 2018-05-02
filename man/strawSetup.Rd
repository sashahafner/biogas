\name{strawSetup}
\alias{strawSetup}
\docType{data}
\title{
  Setup Details for Batch Reactors
}
\description{
  Description of reactor substrates and information on reactor, substrates and, inoculum initial masses for 12 anaerobic reactors with straw for substrate. 
}
\usage{data("setup")}
\format{
  A data frame with 12 observations on the following 6 variables:
  \describe{
    \item{\code{bottle}}{identification code, a unique value for each reactor in the dataset. Integer with all values from 1 to 12.}
    \item{\code{treatment}}{treatment of the substrate (ground straw), a factor.}
    \item{\code{start}}{starting date and time, when the reactors were set up.}
    \item{\code{sub.mass}}{ mass of substrate added to the reactor at the start in g, a numeric vector.}
    \item{\code{inoc.mass}}{mass of inoculum added to the reactor at the start in g, a numeric vector.}
    \item{\code{headspace}}{bottle headspace volume, in mL. A numeric vector.}
}
}
\details{
  These data are meant to be example data for \code{\link{summBg}}.
  Reactors were ca. 600 mL glass serum bottles with butyl rubber septa and screw caps. 
  Masses were measured with an electronic balance to 10 mg.
  Data in \code{strawPressure}, \code{strawMass}, and \code{strawComp} are from the same reactors.
}
\source{
  Measurements by Charlotte Rennuit.
}
\examples{
data(setup)
}
\keyword{datasets}
\concept{biogas}
