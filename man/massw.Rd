\name{massw}
\alias{massw}
\docType{data}
\title{
  Mass Change of Batch Reactors
}
\description{
  Mass measurements for 9 batch anaerobic reactors in a \dQuote{wide} format (separate columns for initial and final mass).
}
\usage{data("massw")}
\format{
  A data frame with 9 observations on the following 3 variables.
  \describe{
    \item{\code{id}}{identification code, a unique value for each reactor in the dataset. A factor with levels \code{2_1}, \code{2_2}, \code{2_3}, \code{2_4}, \code{2_5}, \code{2_6}, \code{2_10}, \code{2_11}, and \code{2_12} }
    \item{\code{start}}{total reactor mass at the start of its incubation in g, a numeric vector.}
    \item{\code{end}}{total reactor mass at the end of its incubation in g, a numeric vector.}
  }
}
\details{
  These data are meant to be example data for application of \code{\link{mass2vol}}, for gravimetric determination of biogas production.
  Reactors were 500 mL glass serum bottles with butyl rubber septa and screw caps. 
  Masses were measured with an electronic balance to 100 mg.
  These are the same data as in \code{mass}, but simply in a \dQuote{wide} format.
  Data in \code{vol}, \code{mass}, \code{massw}, \code{comp}, and \code{setup} are from the same reactors.
}
\source{
  Measurements by Charlotte Rennuit and Sasha Hafner.
}
\examples{
data(massw)
}
\keyword{datasets}
\concept{biogas}
