\name{s3volw}
\alias{s3volw}
\docType{data}
\title{
  Biogas Volume from Batch Reactors
}
\description{
  Biogas volume measurements from three batch bottles, organized in a \dQuote{wide} format.
}
\usage{data("vol")}
\format{
  A data frame with 7 observations on the following 5 variables:
  \describe{
    \item{\code{time.d}}{elapsed time of mass measurements (from bottle setup) in days, a numeric vector.}
    \item{\code{D}}{volume of biogas removed at \code{time} from bottle \dQuote{D} in mL, a numeric vector.}
    \item{\code{E}}{volume of biogas removed at \code{time} from bottle \dQuote{E} in mL, a numeric vector.}
    \item{\code{F}}{volume of biogas removed at \code{time} from bottle \dQuote{F} in mL, a numeric vector.}
  }
}
\details{
  These data are meant to be example data for \code{\link{cumBg}}.
  Reactors were 300 mL glass serum bottles with butyl rubber septa and screw caps. 
  Volume was measured using syringes.
  Data in \code{s3voll}, \code{s3compl}, \code{s3volw}, \code{s3compw}, and \code{s3lcombo} are from the same bottles.
}
\source{
  Sasha D. Hafner. Measurements by Johanna Maria Pedersen.
}
\examples{
data(vol)
}
\keyword{datasets}
\concept{biogas}
