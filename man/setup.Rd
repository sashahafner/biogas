\name{setup}
\alias{setup}
\docType{data}
\title{
  Setup Details for Batch Reactors
}
\description{
  Description of reactor substrates and information on reactor, substrates and, inoculum initial masses for twelve anaerobic reactors. 
}
\usage{data("setup")}
\format{
  A data frame with 12 observations on the following 11 variables:
  \describe{
    \item{\code{id}}{identification code, a unique value for each reactor in the dataset. A factor with levels \code{2_1}, \code{2_2}, \code{2_3}, \code{2_4}, \code{2_5}, \code{2_6}, \code{2_7}, \code{2_8}, \code{2_9}, \code{2_10}, \code{2_11}, and \code{2_12} }
    \item{\code{descrip}}{description of substrate, a factor with levels \code{A} \code{B} \code{inoc}.}
    \item{\code{msub}}{ mass of substrate added to the reactor at the start in g, a numeric vector.}
    \item{\code{minoc}}{mass of inoculum added to the reactor at the start in g, a numeric vector.}
    \item{\code{mvs.sub}}{mass of volatile solids (VS) of substrate added to the reactor at the start in g, a numeric vector.}
    \item{\code{mvs.inoc}}{mass of volatile solids (VS) of inoculum added to the reactor at the start in g, a numeric vector.}
    \item{\code{mcod.sub}}{mass of chemical oxygen demand (COD) of substrate added to the reactor at the start in g oxygen, a numeric vector.}
    \item{\code{mcod.inoc}}{mass of chemical oxygen demand (COD) of inoculum added to the reactor at the start in g oxygen, a numeric vector.}
    \item{\code{m.tot}}{total mass added to the reactor at the start in g, a numeric vector.}
    \item{\code{mvs.tot}}{total mass of VS in the reactor at the start in g, a numeric vector.}
    \item{\code{mcod.tot}}{total mass of COD in the reactor at the start in g oxygen, a numeric vector.}
}
}
\details{
  These data are meant to be example data for \code{\link{summBg}}.
  Reactors were 500 mL glass serum bottles with butyl rubber septa and screw caps. 
  Data in \code{vol}, \code{mass}, \code{massw}, \code{comp}, and \code{setup} are from the same reactors.
}
\source{
  Measurements made by Charlotte Rennuit and Ali Heidarzadeh Vazifehkhoran
}
\examples{
data(setup)
}
\keyword{datasets}
\concept{biogas}
