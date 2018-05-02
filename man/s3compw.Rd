\name{s3compw}
\alias{s3compw}
\docType{data}
\title{
  Methane Content of Biogas from Batch Reactors
}
\description{
  Methane content (biogas composition) measurements from three batch bottles, organized in a \dQuote{wide} format.
}
\usage{data("vol")}
\format{
  A data frame with 5 observations on the following 4 variables:
  \describe{
    \item{\code{time.d}}{elapsed time of mass measurements (from bottle setup) in days, a numeric vector.}
    \item{\code{D}}{biogas methane content at \code{time} from bottle \dQuote{D} as mole fraction, a numeric vector.}
    \item{\code{E}}{biogas methane content at \code{time} from bottle \dQuote{E} as mole fraction, a numeric vector.}
    \item{\code{F}}{biogas methane content at \code{time} from bottle \dQuote{F} as mole fraction, a numeric vector.}
  }
}
\details{
  These data are meant to be example data for \code{\link{cumBg}}.
  Reactors were 300 mL glass serum bottles with butyl rubber septa and screw caps. 
  Substrate was municipal wastewater sludge.
  Methane and carbon dioxide contents were determined by gas chromatography using a thermal conductivity detector and normalised so methane and carbon dioxide sum to 1.0.
  Data in \code{s3voll}, \code{s3compl}, \code{s3volw}, \code{s3compw}, and \code{s3lcombo} are from the same bottles.
}
\source{
  Sasha D. Hafner. Measurements by Johanna Maria Pedersen.
}
\examples{
data(s3compw)
}
\keyword{datasets}
\concept{biogas}
