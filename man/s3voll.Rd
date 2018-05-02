\name{s3voll}
\alias{s3voll}
\docType{data}
\title{
  Biogas Volume from Batch Bottles
}
\description{
  Biogas volume measurements from three batch bottles, organized in a \dQuote{long} format.
}
\usage{data("s3voll")}
\format{
  A data frame with 21 observations on the following 4 variables:
  \describe{
    \item{\code{id}}{identification code, a unique value for each bottle in the dataset. }
    \item{\code{time.d}}{elapsed time of mass measurements (from bottle setup) in days, a numeric vector.}
    \item{\code{vol.ml}}{volume of biogas removed at \code{time} in mL, a numeric vector.}
    \item{\code{cvol.ml}}{cumulative sum of \code{vol}.}
  }
}
\details{
  These data are meant to be example data for \code{\link{cumBg}}.
  Reactors were 300 mL glass serum bottles with butyl rubber septa and screw caps. 
  Substrate was municipal wastewater sludge.
  Volume was measured using syringes.
  Data in \code{s3voll}, \code{s3compl}, \code{s3volw}, \code{s3compw}, and \code{s3lcombo} are from the same bottles.
}
\source{
  Sasha D. Hafner. Measurements by Johanna Maria Pedersen.
}
\examples{
data(s3voll)
}
\keyword{datasets}
\concept{biogas}
