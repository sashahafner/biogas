\name{UQGDSetup}
\alias{UQGDSetup}
\docType{data}
\title{
  Mass Change of Batch Reactors
}
\description{
  Mass measurements for 9 batch anaerobic bottles.
}
\usage{data("UQGDSetup")}
\format{
  A data frame with 135 observations on the following variables.
\describe{
    \item{\code{id}}{identification code, a unique value for each bottle in the dataset. A factor.
    \item{\code{descrip}}{description of substrate, a factor with levels \code{Inoculum}, \code{Cellulose}, \code{Substrate C}.}
    \item{\code{m.inoc}}{mass of inoculum added to the bottle at the start in g. Numeric.}
    \item{\code{m.sub}}{mass of substrate added to the bottle at the start in g. Numeric.}
    \item{\code{m.tot}}{total mass added to the bottle at the start in g. Numeric.}
    \item{\code{m.sub.vs}}{mass of volatile solids (VS) of substrate added to the bottle at the start in g. Numeric.}
    \item{\code{vol.hs}}{bottle headspace in mL. Numeric.}
    \item{\code{isr}}{VS-based inoculum-to-substrate ratio.}
  }
}
\details{
  These data are meant to be example data for \code{\link{calcBgGD}}, for gas density-based determination of biogas production (GD-BMP).
  Reactors were glass serum bottles with butyl rubber septa.
  Data in \code{UQGDBiogas} are from the same bottles.
}
\source{
  Measurements by Sergi Astals.
}
\examples{
data(UQGDSetup)
}
\keyword{datasets}
\concept{biogas}
