\name{UQGravSetup}
\alias{UQGravSetup}
\docType{data}
\title{
  BMP Bottle Setup Information
}
\description{
  Details on bottle contents and more from a gravimetric BMP test.
}
\usage{data("UQGravSetup")}
\format{
  A data frame with 9 observations on the following variables.
\describe{
    \item{\code{id}}{identification code, a unique value for each bottle in the dataset. A factor.}
    \item{\code{descrip}}{description of substrate, a factor with levels \code{Inoculum}, \code{Cellulose}, \code{Substrate C}.}
    \item{\code{m.inoc}}{mass of inoculum added to the bottle at the start in g. Numeric.}
    \item{\code{m.sub}}{mass of substrate added to the bottle at the start in g. Numeric.}
    \item{\code{m.tot}}{total mass added to the bottle at the start in g. Numeric.}
    \item{\code{vol.hs}}{bottle headspace in mL. Numeric.}
    \item{\code{m.sub.vs}}{mass of volatile solids (VS) of substrate added to the bottle at the start in g. Numeric.}
    \item{\code{m.inoc.vs}}{mass of volatile solids (VS) of inoculum added to the bottle at the start in g. Numeric.}
    \item{\code{isr}}{VS-based inoculum-to-substrate ratio.}
  }
}
\details{
  These data are meant to be example data for \code{\link{calcBgGrav}}, for gravimetric determination of biogas production.
  Reactors were glass serum bottles with butyl rubber septa.
  Data in \code{UQGravBiogas} are from the same bottles.
}
\source{
  Measurements were made by Sergi Astals and are described in the following paper.

  Justesen, C.G., Astals, S., Mortensen, J.R., Thorsen, R., Koch, K., Weinrich, S., Triolo, J.M., Hafner, S.D. 2019. Development and validation of a low-cost gas density method for measuring biochemical potential (BMP) \emph{Water (MDPI)} \bold{11(12)}: 2431.
}
\examples{
data(UQGravSetup)
}
\keyword{datasets}
\concept{biogas}
