\name{vol2}
\alias{vol2}
\docType{data}
\title{
  Biogas Volume from Batch Reactors
}
\description{
  Biogas volume measurements from 15 batch reactors.
}
\usage{data("vol2")}
\format{
  A data frame with 216 observations on the following 4 variables:
  \describe{
    \item{\code{bottle}}{identification code, a unique value for each reactor in the dataset. A factor with levels \code{1_1}, \code{1_2}, \code{1_3}, \code{2_1}, \code{2_2}, \code{2_2}, ... through \code{5_3}. The first number indicates the sample, the second the replicate.}
    \item{\code{days}}{elapsed time of mass measurements (from reactor setup) in days, a numeric vector.}
    \item{\code{meas.vol}}{volume of biogas removed at \code{days} in mL, a numeric vector}
  }
}
\details{
  These data are meant to be example data for multiple functions, e.g., \code{\link{stdVol}}, \code{\link{cumBg}}, or \code{\link{summBg}}.
  Reactors were 500 mL or 1000 mL glass serum bottles with butyl rubber septa and screw caps. 
  Volume was measured using syringes.
  Data in \code{vol2}, \code{xCH42}, and \code{setup2} are from the same reactors.
}
\source{
  Measurements by Ali Heidarzadeh Vazifehkhoran.
}
\examples{
data(vol2)
}
\keyword{datasets}
\concept{biogas}
