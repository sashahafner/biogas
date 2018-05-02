\name{vol2mass}
\alias{vol2mass}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calculate Mass of Biogas
}
\description{
  \code{vol2mass} calculates the mass of biogas removed from a reactor, based on its composition, temperature, and pressure.
  This function is the inverse of \code{mass2vol}.
}
\usage{
vol2mass(volBg, xCH4, temp.hs, temp.vol, pres.hs, pres.vol, 
  unit.temp = getOption('unit.temp', 'C'), 
  unit.pres = getOption('unit.pres', 'atm'), 
  rh.hs = 1, rh.vol = 1)
}

\arguments{
  \item{volBg}{
  measured (not standardised) biogas volume in mL.
  Numeric vector.
}
  \item{xCH4}{
  mole fraction of methane within biogas (dry, methane and carbon dioxide only).
  Numeric vector.
}
  \item{temp.hs}{
    temperature of biogas just prior to removal, in the units specified in \code{unit.temp} (default of degrees C).
    Numeric vector.
}  
\item{temp.vol}{
    temperature of biogas at the time of volume measurement, in the units specified in \code{unit.temp} (default of degrees C).
    Numeric vector.
}
  \item{pres.hs}{
    pressure of biogas just prior to removal, in the units specified in \code{unit.pres} (default of atm).
    Identical to \code{pres} argument in \code{\link{mass2vol}}.
    Numeric vector.
}
  \item{pres.vol}{
    pressure of gas at the time of measurement in atm by default (see \code{unit.pres}).
    Identical to \code{pres} argument of \code{\link{stdVol}}.
    Numeric vector.
}
  \item{unit.pres}{
    pressure units.
    Options are \code{"atm"} (the default), \code{"Pa"}, \code{"kPa"}, \code{"hPa"}, and \code{"bar"}.
    Length-one character vector.
}
  \item{unit.temp}{
    temperature units.
    Options are \code{"C"} (degrees Celcius, the default), \code{"F"}, and \code{"K"}.
    Length-one character vector.
}
  \item{rh.hs}{
    relative humidity of the reactor headspace just prior to biogas removal.
    Length one numeric vector between zero and 1.0.
  }
  \item{rh.vol}{
    relative humidity of the biogas at the time of volume measurement.
    Length one numeric vector between zero and 1.0.
  }
}
\details{
  This function is vectorized. 
  Argument elements will be recycled as needed.
  Note that this function is conceptually but not numerically the inverse of \code{mass2vol}, because the \code{volBg} argument here is not standardised, and is assumed to be saturated with water vapor just prior to removal.
  The mass that is calculated may not be equal to the mass of the biogas at the time of volume measurement--as long as \code{temp.vol} is less than \code{temp.hs}, some of the water lost from the reactor condenses and is not present in the biogas at the time of volume measurement.
  
   Standard values and units of temperature and pressure can be globally set using the function \code{\link{options}}.
}
\value{
  biogas mass in g as a numeric vector.
}
\references{
  Hafner, S.D., Rennuit, C., Triolo, J.M., Richards, B.K. In review. A gravimetric method for measuring biogas production. Biomass and Bioenergy.
}
\author{
  Sasha D. Hafner and Charlotte Rennuit
}

\seealso{
  \code{\link{mass2vol}},
  \code{\link{options}}
}
\examples{
# Mass loss from reactor for 100 mL biogas measured at 20 degrees C 
# and 1.0 atm, with headspace at 1.5 atm and 35 degrees C at the 
# time of biogas exit
vol2mass(100, xCH4 = 0.65, temp.hs = 35, temp.vol = 20, 
         pres.hs = 1.5, pres.vol = 1)

# If the measured volume has already been standardised to dry 
# conditions at 0 C and 1 atm
vol2mass(100, xCH4 = 0.65, temp.hs = 35, temp.vol = 0, 
         pres.hs = 1.5, pres.vol = 1, rh.vol = 0)

# Here vol2mass *is* numerically the inverse of mass2vol
vol2mass(mass2vol(1.234, xCH4 = 0.65, temp = 35, pres = 1.5, 
                  value = "Bg"), 
         xCH4 = 0.65, temp.hs = 35, temp.vol = 0, pres.hs = 1.5, 
         pres.vol = 1, rh.vol = 0)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\concept{biogas}
