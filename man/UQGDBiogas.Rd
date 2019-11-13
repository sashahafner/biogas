\name{UQGDBiogas}
\alias{UQGDBiogas}
\docType{data}
\title{
  Mass Change of Batch Reactors
}
\description{
  Mass measurements for 9 batch anaerobic bottles.
}
\usage{data("UQGDBiogas")}
\format{
  A data frame with 135 observations on the following variables.
  \describe{
    \item{\code{id}}{identification code, a unique value for each bottle in the dataset. A factor.
    \item{\code{time.d}}{elapsed time of measurements (from bottle setup) in days, a numeric vector.}
    \item{\code{vol}}{measured biogas volume at the end of a sampling interval, a numeric vector.}
    \item{\code{mass.init}}{total bottle mass prior to biogas venting in g, a numeric vector.}
    \item{\code{mass.final}}{total bottle mass after biogas venting in g, a numeric vector.}
  }
}
\details{
  These data are meant to be example data for \code{\link{calcBgGD}}, for gas density-based determination of biogas production (GD-BMP).
  Reactors were glass serum bottles with butyl rubber septa.
  Masses were measured with an analytical balance.
  Data in \code{UQGDSetup} are from the same bottles.
}
\source{
  Measurements by Sergi Astals.
}
\examples{
data(UQGDBiogas)
}
\keyword{datasets}
\concept{biogas}
