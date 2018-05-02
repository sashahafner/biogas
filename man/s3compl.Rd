\name{s3compl}
\alias{s3compl}
\docType{data}
\title{
  Methane Content of Biogas from Batch Reactors
}
\description{
  Methane content (biogas composition) measurements from three batch bottles, organized in a \dQuote{long} format.
}
\usage{data("s3compl")}
\format{
  A data frame with 14 observations on the following 3 variables:
  \describe{
    \item{\code{id}}{identification code, a unique value for each bottle in the dataset. }
    \item{\code{time.d}}{elapsed time of mass measurements (from bottle setup) in days, a numeric vector.}
    \item{\code{xCH4}}{biogas methane content as a mole fraction, excluding water and all other gases other than carbon dioxide, a numeric vector}
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
data(s3compl)
}
\keyword{datasets}
\concept{biogas}
