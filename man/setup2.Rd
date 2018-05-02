\name{setup2}
\alias{setup2}
\docType{data}
\title{
  Setup Details for Some Batch Reactors
}
\description{
  Information on reactor substrates and substrate and inoculum masses for 15 anaerobic reactors.
}
\usage{data("setup2")}
\format{
  A data frame with 15 observations on the following 4 variables:
  \describe{
    \item{\code{bottle}}{identification code, a unique value for each reactor in the dataset. A factor with levels \code{1_1}, \code{1_2}, \code{1_3}, \code{2_1}, \code{2_2}, \code{2_2}, ... through \code{5_3}. The first number indicates the sample, the second the replicate.}
    \item{\code{description}}{description of substrate, a factor.}
    \item{\code{sub.vs}}{mass of substrate volatile solids (VS) added to the reactor at the start in g, a numeric vector}
    \item{\code{inoc.mass}}{mass of inoculum added to the reactor at the start in g, a numeric vector.}
  }
}
\details{
  These data are meant to be example data for \code{\link{summBg}}.
  Reactors were 500 mL or 1000 mL glass serum bottles with butyl rubber septa and screw caps. 
  Data in \code{vol2}, \code{xCH42}, and \code{setup2} are from the same reactors.
}
\source{
  Measurements made by Ali Heidarzadeh Vazifehkhoran
}
\examples{
data(setup2)
}
\keyword{datasets}
\concept{biogas}
