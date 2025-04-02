\name{UQGravBiogas}
\alias{UQGravBiogas}
\docType{data}
\title{
  Mass Loss and Biogas Composition from BMP bottles
}
\description{
  Gravimetric biochemical methane potential measurements for 9 batch anaerobic bottles.
}
\usage{data("UQGravBiogas")}
\format{
  A data frame with 135 observations on the following variables.
  \describe{
    \item{\code{id}}{identification code, a unique value for each bottle in the dataset. A factor.}
    \item{\code{day}}{elapsed time of measurements (from bottle setup) in days, a numeric vector.}
    \item{\code{mass.init}}{total bottle mass prior to biogas venting in g, a numeric vector.}
    \item{\code{mass.final}}{total bottle mass after biogas venting in g, a numeric vector.}
    \item{\code{xCH4}}{biogas methane content as a mole fraction (normalized so methane and carbon dioxide sum to 1.0), a numeric vector.}
  }
}
\details{
  These data are meant to be example data for \code{\link{calcBgGrav}}, for gravimetric determination of biogas production.
  Reactors were glass serum bottles with butyl rubber septa.
  Masses were measured with an analytical balance.
  Data in \code{UQGravSetup} are from the same bottles.
}
\source{
  Measurements were made by Sergi Astals and are described in the following paper.

  Justesen, C.G., Astals, S., Mortensen, J.R., Thorsen, R., Koch, K., Weinrich, S., Triolo, J.M., Hafner, S.D. 2019. Development and validation of a low-cost gas density method for measuring biochemical potential (BMP) \emph{Water (MDPI)} \bold{11(12)}: 2431.
}

\examples{
data(UQGravBiogas)
}

\keyword{datasets}
\concept{biogas}
