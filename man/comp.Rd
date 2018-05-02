\name{comp}
\alias{comp}
\docType{data}
\title{
  Methane Content of Biogas
}
\description{
  Methane content (biogas composition) measurements from nine batch reactors.
}
\usage{data("comp")}
\format{
  A data frame with 132 observations on the following 4 variables.
  \describe{
    \item{\code{id}}{identification code, a unique value for each reactor in the dataset. A factor with levels \code{2_1}, \code{2_2}, \code{2_3}, \code{2_4}, \code{2_5}, \code{2_6}, \code{2_7}, \code{2_8}, \code{2_9}, \code{2_10}, \code{2_11}, and \code{2_12} }
    \item{\code{date.time}}{date and time of mass measurement, a POSIXct object.}
    \item{\code{days}}{elapsed time of mass measurements (from reactor setup) in days, a numeric vector.}
    \item{\code{xCH4}}{biogas methane content as a mole fraction, excluding water and all other gases other than carbon dioxide, a numeric vector}
  }
}
\details{
  These data are meant to be example data for multiple functions, e.g., \code{\link{interp}}, \code{\link{cumBg}}, or \code{\link{summBg}}.
  Reactors were 500 mL glass serum bottles with butyl rubber septa and screw caps. 
  Methane and carbon dioxide contents were determined by gas chromatography using a thermal conductivity detector and normalised so methane and carbon dioxide sum to 1.0.
  Data in \code{vol}, \code{mass}, \code{comp}, and \code{setup} are from the same reactors.
}
\source{
  Measurements by Charlotte Rennuit and Ali Heidarzadeh Vazifehkhoran.
}
\examples{
data(comp)
}
\keyword{datasets}
\concept{biogas}
