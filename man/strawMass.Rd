\name{strawMass}
\alias{strawMass}
\docType{data}
\title{
  Mass Change of Batch Reactors
}
\description{
  Mass measurements for 12 batch anaerobic reactors with ground straw as a substrate.
}
\usage{data("mass")}
\format{
  A data frame with 89 observations on the following 4 variables.
  \describe{
    \item{\code{bottle}}{identification code, a unique value for each reactor in the dataset. Integer with all values from 1 to 12.}
    \item{\code{date.time}}{date and time of mass measurement, a POSIXct object.}
    \item{\code{time}}{elapsed time of mass measurements (from reactor setup) in days, a numeric vector.}
    \item{\code{mass}}{total reactor mass in g, a numeric vector.}
  }
}
\details{
  These data are meant to be example data for \code{\link{mass2vol}} or  \code{\link{cumBg}}, for gravimetric determination of biogas production.
  Reactors were ca. 600 mL glass serum bottles with butyl rubber septa and screw caps. 
  Masses were measured with an electronic balance to 10 mg.
  Data in \code{strawPressure}, \code{strawSetup}, and \code{strawComp} are from the same reactors.
}
\source{
  Measurements by Charlotte Rennuit.
}
\examples{
data(strawMass)
}
\keyword{datasets}
\concept{biogas}
