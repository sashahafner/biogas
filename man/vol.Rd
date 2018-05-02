\name{vol}
\alias{vol}
\docType{data}
\title{
  Biogas Volume from Batch Reactors
}
\description{
  Biogas volume measurements from twelve batch reactors.
}
\usage{data("vol")}
\format{
  A data frame with 288 observations on the following 4 variables:
  \describe{
    \item{\code{id}}{identification code, a unique value for each reactor in the dataset. A factor with levels \code{2_1}, \code{2_2}, \code{2_3}, \code{2_4}, \code{2_5}, \code{2_6}, \code{2_7}, \code{2_8}, \code{2_9}, \code{2_10}, \code{2_11}, and \code{2_12} }
    \item{\code{date.time}}{date and time of mass measurement, a POSIXct object.}
    \item{\code{days}}{elapsed time of mass measurements (from reactor setup) in days, a numeric vector.}
    \item{\code{vol}}{volume of biogas removed at \code{date.time} in mL, a numeric vector.}
  }
}
\details{
  These data are meant to be example data for multiple functions, e.g., \code{\link{stdVol}}, \code{\link{cumBg}}, or \code{\link{summBg}}.
  Reactors were 500 mL glass serum bottles with butyl rubber septa and screw caps. 
  Volume was measured using syringes.
  Data in \code{vol}, \code{mass}, \code{xCH4}, and \code{setup} are from the same reactors.
}
\source{
  Measurements by Charlotte Rennuit and Ali Heidarzadeh Vazifehkhoran.
}
\examples{
data(vol)
}
\keyword{datasets}
\concept{biogas}
