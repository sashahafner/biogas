\name{feedSetup}
\alias{feedSetup}
\docType{data}
\title{
  Setup Details for Batch Reactors
}
\description{
  Information on bottle id, substrates and, inoculum initial masses for 12 anaerobic bottles with animal feed ingredients for substrate.
}
\usage{data("feedSetup")}
\format{
  A data frame with 12 observations on the following 3 variables:
  \describe{
    \item{\code{descrip}}{description of bottle contents.}
    \item{\code{id}}{identification code, a unique value for each bottle in the dataset. Numeric with all values from 1 to 12.}
    \item{\code{m.inoc}}{mass of inoculum added to the bottle at the start in g, a numeric vector.}
    \item{\code{m.sub.vs}}{mass of volatile solids (VS) of substrate added to the bottle at the start in g, a numeric vector.}
}
}
\details{
  These data are meant to be example data for \code{\link{summBg}}.
  Inoculum was digestate from a stable laboratory reactor (400 L) operated at mesophilic temperatures (38 degrees C) and a retention time of ca. 98 days
  Substrates were cellulose, wheat straw (WS), and animal feed ingredients (FI).
  Additinally, 1 mL each of a vitamin mixture and trace element solution was added to each BMP bottle.

  BMP tests were operated with a total volume of ca. 400 mL at 38 degrees C. 

  Data in \code{feedVol} and \code{feedSetup} are from the same bottles.
}
\source{
  Data were originally collected by Soeren Weinrich and others at DBFZ in Leipzig Germany.
}
\examples{
data(feedSetup)
}
\keyword{datasets}
\concept{biogas}
