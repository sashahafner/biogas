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
    \item{\code{xCH4}}{biogas methane content as a mole fraction, excluding water, a numeric vector}
    \item{\code{xCO2}}{biogas carbon dioxide content as a mole fraction, excluding water, a numeric vector}
    \item{\code{xCH4n}}{biogas methane content as a mole fraction in dry biogas, normalized so the sum of mole fractions of CH$_4$ and CO$_2$ is unity, a numeric vector.}

  }
}
\details{
  These data are meant to be example data for \code{\link{cumBg}} and \code{\link{calcBgMan}} for respectively, gravimetric and manometric determination of biogas production.
  BMP test were carried out in 160 mL serum bottles sealed with a butyl rubber septum retained with an aluminum crimp seal at mesophilic conditions (37 degrees C). 
  Substrate was primary wastewater sludge.
  
  Cumulative methane production was measured from the same set of bottles by both manometric and gravimetric methods. 
  BMP was evaluated at 18 days
  At each sampling event, headspace pressure was measured using a manometer and composition (CH4 and CO2) with a gas chromatograph). 
  All reported pressure measurements are gauge unless stated otherwise.
  After each sampling event, the headspace of the serum bottles (including blanks) was vented to near atmospheric pressure.
  
  Data in \code{sludgeTwoBiogas} and \code{sludgeTwoSetup} are from the same batch bottles.
}
\source{
  Data were originally collected by Sergi Astals at the University of Queensland and are described in the following paper.
  Hafner, S.D., Astals, S. 2019.Systematic error in manometric measurement of biochemical methane potential: Sources and solutions. \emph{Waste Management} \bold{91}, 147-155.
}
\examples{
data(sludgeTwoBiogas)
}
\keyword{datasets}
\concept{biogas}
