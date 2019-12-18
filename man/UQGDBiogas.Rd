\name{UQGDBiogas}
\alias{UQGDBiogas}
\docType{data}
\title{
  Biogas Volume and Mass Loss from BMP bottles
}
\description{
  Gas density biochemical methane potential (GD-BMP) measurements for 9 batch anaerobic bottles.
}
\usage{data("UQGDBiogas")}
\format{
  A data frame with 135 observations on the following variables.
  \describe{
    \item{\code{id}}{identification code, a unique value for each bottle in the dataset. A factor.}
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
  Measurements were made by Sergi Astals and are described in the following paper.

  Justesen, C.G., Astals, S., Mortensen, J.R., Thorsen, R., Koch, K., Weinrich, S., Triolo, J.M., Hafner, S.D. 2019. Development and validation of a low-cost gas density method for measuring biochemical potential (BMP) \emph{Water (MDPI)} \bold{11(12)}: 2431.
}

\examples{
data(UQGDBiogas)
}

\keyword{datasets}
\concept{biogas}
