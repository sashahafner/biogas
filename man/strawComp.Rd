\name{strawComp}
\alias{strawComp}
\docType{data}
\title{
  Methane Content of Biogas
}
\description{
  Methane content (biogas composition) measurements from 12 anaerobic batch reactors with straw for substrate.
}
\usage{data("comp")}
\format{
  A data frame with 63 observations on the following 4 variables.
  \describe{
    \item{\code{bottle}}{identification code, a unique value for each reactor in the dataset. Integer with all values from 1 to 12.}
    \item{\code{date.time}}{date and time of mass measurement, a POSIXct object.}
    \item{\code{time}}{elapsed time of mass measurements (from reactor setup) in days, a numeric vector.}
    \item{\code{xCH4}}{biogas methane content as a mole fraction, excluding water and all other gases other than carbon dioxide, a numeric vector}
  }
}
\details{
  These data are meant to be example data for \code{\link{cumBg}}.
  Reactors were ca. 600 mL glass serum bottles with butyl rubber septa and screw caps. 
  Pressure was measured using an electronic manometer.
  Data in \code{strawMass}, \code{strawSetup}, and \code{strawPressure} are from the same reactors.
}
\source{
  Measurements by Charlotte Rennuit.
}
\examples{
data(strawComp)
}
\keyword{datasets}
\concept{biogas}
