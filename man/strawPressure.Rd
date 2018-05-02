\name{strawPressure}
\alias{strawPressure}
\docType{data}
\title{
  Headspace Pressure in Batch Reactors
}
\description{
  Interval-based measurements of headspace pressure in 12 anaerobic batch reactors with straw as the substrate.
}
\usage{data("strawPressure")}
\format{
  A data frame with 72 observations on the following 5 variables:
  \describe{
    \item{\code{bottle}}{identification code, a unique value for each reactor in the dataset. Integer with all values from 1 to 12.}
    \item{\code{date.time}}{date and time of mass measurement, a POSIXct object.}
    \item{\code{time}}{elapsed time of mass measurements (from reactor setup) in days, a numeric vector.}
    \item{\code{pres}}{absolute bottle headspace pressure at the stated time, in kPa. Biogas accumulated in bottles from the previous time.}
    \item{\code{pres.resid}}{absolute bottle headspace pressure after venting, in kPa.}
  }
}
\details{
  These data are meant to be example data for \code{\link{cumBg}}.
  Reactors were ca. 600 mL glass serum bottles with butyl rubber septa and screw caps. 
  Pressure was measured using an electronic manometer.
  Data in \code{strawMass}, \code{strawSetup}, and \code{strawComp} are from the same reactors.
}
\source{
  Measurements by Charlotte Rennuit.
}
\examples{
data(vol)
}
\keyword{datasets}
\concept{biogas}
