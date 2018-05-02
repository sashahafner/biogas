\name{mass}
\alias{mass}
\docType{data}
\title{
  Mass Change of Batch Reactors
}
\description{
  Mass measurements for 9 batch anaerobic reactors.
}
\usage{data("mass")}
\format{
  A data frame with 18 observations on the following 4 variables.
  \describe{
    \item{\code{id}}{identification code, a unique value for each reactor in the dataset. A factor with levels \code{2_1}, \code{2_2}, \code{2_3}, \code{2_4}, \code{2_5}, \code{2_6}, \code{2_10}, \code{2_11}, and \code{2_12} }
    \item{\code{date.time}}{date and time of mass measurement, a POSIXct object.}
    \item{\code{days}}{elapsed time of mass measurements (from reactor setup) in days, a numeric vector.}
    \item{\code{mass}}{total reactor mass in g, a numeric vector.}
    \item{\code{when}}{when was measurement taken? A character vector with two values.}
  }
}
\details{
  These data are meant to be example data for \code{\link{mass2vol}}, for gravimetric determination of biogas production.
  Reactors were 500 mL glass serum bottles with butyl rubber septa and screw caps. 
  Masses were measured with an electronic balance to 100 mg.
  Data in \code{vol}, \code{mass}, \code{massw}, \code{comp}, and \code{setup} are from the same reactors.
}
\source{
  Measurements by Charlotte Rennuit and Sasha Hafner.
}
\examples{
data(mass)
}
\keyword{datasets}
\concept{biogas}
