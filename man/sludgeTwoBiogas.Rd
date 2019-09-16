\name{sludgeTwoBiogas}
\alias{sludgeTwoBiogas}
\docType{data}
\title{
  Headspace Pressure, Mass measurements, and Methane and Carbondioxid Content from Batch Bottles
}
\description{
  Interval-based measurements of headspace pressure, mass, and methane and carbondioxide content from anaerobic batch bottles, organized in a \dQuote{longcombo} format.
}
\usage{data("sludgeTwoBiogas")}
\format{
  A data frame with 224 observations on the following 8 variables:
  \describe{
    \item{\code{id}}{identification code, a unique value for each bottle in the dataset.}
    \item{\code{time.d}}{elapsed time of mass and pressure measurements in days, a numeric vector.}
    \item{\code{pres}}{bottle headspace pressure at \code{time} in mbar (gauge), a numeric vector}
    \item{\code{mass.init}}{total reactor mass at the start of its incubation in g, a numeric vector.}
    \item{\code{mass.final}}{total reactor mass at the end of its incubation in g, a numeric vector.}
    \item{\code{xCH4}}{biogas methane content as a mole fraction, excluding water and all other gases other than carbon dioxide, a numeric vector}
    \item{\code{xCO2}}{biogas carbon dioxide content as a mole fraction, excluding water and all other gases other than methane, a numeric vector}
    \item{\code{xCH4n}}{biogas methane content as a mole fraction in dry biogas, normalized so the sum of mole fractions of CH$_4$ and CO$_2$ sum to unity, a numeric vector.}

  }
}
\details{
  These data are meant to be example data for \code{\link{cumBg} and \code{\link{cumBgMan}} for respectively, gravimetric and manometric determination of biogas production.
  Substrate was primary wastewater sludge.
  
  Mass and pressure were measured using manual gravimetric and manometric method, respectively.
  Methane and carbon dioxide contents were determined by gas chromatography using a thermal conductivity detector and normalised so methane and carbon dioxide sum to 1.0. % NTS: make sure this was the approach. I have just assumed, as this is normally used. 
  Data in \code{sludgeTwoBiogas} and \code{sludgeTwoSetup} are from the same batch bottles.
}
\source{
  Sasha D. Hafner. Data were originally collected by Sergi Astals at the University of Queensland.
}
\examples{
data(sludgeTwoBiogas)
}
\keyword{datasets}
\concept{biogas}
