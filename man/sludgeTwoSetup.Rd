\name{sludgeTwoSetup}
\alias{sludgeTwoSetup}
\docType{data}
\title{
  Setup Details for Batch Reactors
}
\description{
  Description of reactor substrates and information on bottle, substrates, inoculum initial masses, and headspace volume for 18 anaerobic bottles with primary wastewater sludge for substrate.
}
\usage{data("sludgeTwoSetup")}
\format{
  A data frame with 18 observations on the following 5 variables:
  \describe{
    \item{\code{id}}{identification code, a unique value for each bottle in the dataset. Numeric with all values within a range of 1-21.}
    \item{\code{descrip}}{treatment of the substrate (primary wastewater sludge), a character.}
    \item{\code{vol.hs}}{bottle headspace volume, in mL. A numeric vector.}
    \item{\code{m.inoc}}{mass of inoculum added to the bottle at the start in g, a numeric vector.}
    \item{\code{m.sub.vs}}{ mass of volatile solids (VS) of substrate added to the reactor at the start in g, a numeric vector.}
  }
}
\details{
  These data are meant to be example data for \code{\link{summBg}}.
  Substrate was primary wastewater sludge.
  
  BMP test were carried out in 160 mL serum bottles sealed with a butyl rubber septum retained with an aluminum crimp seal at mesophilic conditions (37 degrees C). 
  All bottles contained inoculum and the amount of primary sludge required to achieve an inoculum-to-substrate ratio (ISR) of 2 (VS basis). 
  Bottles containing only inoculum (blanks) were used to correct for the background CH4 potential of the inoculum (endogenous CH4 production). 
  
  Data in \code{sludgeTwoBiogas} and \code{sludgeTwoSetup} are from the same batch bottles.
}
\source{
  Data were originally collected by Sergi Astals at the University of Queensland and are described in the paper listed below.
  Hafner, S.D., Astals, S. 2019.Systematic error in manometric measurement of biochemical methane potential: Sources and solutions. \emph{Waste Management} \bold{91}, 147-155.
}
\examples{
data(sludgeTwoSetup)
}
\keyword{datasets}
\concept{biogas}
