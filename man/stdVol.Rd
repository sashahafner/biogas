\name{stdVol}
\alias{stdVol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Correct Gas Volume to 'Standard' Conditions
}
\description{
  \code{stdVol} corrects gas volumes to dry conditions at a specified temperature and pressure. 
}
\usage{
stdVol(vol, temp, pres, rh = 1, 
       temp.std = getOption('temp.std', as.numeric(NA)), 
       pres.std = getOption('pres.std', as.numeric(NA)), 
       unit.temp = getOption('unit.temp', 'C'), 
       unit.pres = getOption('unit.pres', 'atm'), 
       std.message = TRUE, warn = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vol}{
    measured gas volume at \code{temp} temperature, \code{pres} pressure, and \code{rh} relative humidity.
    A numeric vector.
}
  \item{temp}{
    temperature of gas in degrees C by default (see \code{unit.temp}).
    A numeric vector.
}
  \item{pres}{
    pressure of gas in atm by default (see \code{unit.pres}).
    A numeric vector.
}
  \item{rh}{
    relative humidity of the gas (\code{0<= rh <= 1}).
    Typically should be 1.0 (the default).
    A numeric vector.
}
  \item{temp.std}{
    "standard" temperature.
    Default value is 0 degrees C.
}
  \item{pres.std}{
    "standard" pressure.
    Default value is 1.0 atm.
}
  \item{unit.pres}{
    pressure units for \code{pres} and \code{pres.std} arguments.
    Options are \code{"atm"} (the default), \code{"Pa"}, \code{"kPa"}, \code{"hPa"}, and \code{"bar"}.
}
  \item{unit.temp}{
    temperature units for \code{temp} and \code{temp.std} arguments.
    Options are \code{"C"} (degrees Celcius), \code{"F"} (degrees Fahrenheit), and \code{"K"} (Kelvin).
}
  
  \item{std.message}{
    should a message with the standard conditions be displayed? Default is TRUE.
}
  
  \item{warn}{
    if \code{TRUE}, will return a warning if temperature is below 0 C  or above 100 C, or if pressure is below 50 kPa or above 150 kPa.
}
}
\details{
  Standardisation is done in a two-step process following Hafner et al. (2015).
  First, the contribution of water vapor is removed and volume is corrected to the standard pressure:

  vd = vm*(pm - rh*pw)/ps

  where vd = dry volume at standard pressure ps, vm = measured volume at pressure pm, pw = saturation vapor pressure of water (all pressures in Pa), and rh = relative humidity (\code{rh} above).
  Water vapor pressure pw is calculated from the Magnus form equation given in Alduchov and Eskridge (1996) (Eqs. (21) and (22)).
  In the second step, the volume is adjusted for temperature.

  vs = vd*Ts/T  

  where vs = standardised volume and Ts = standardisation temperature (K, converted from \code{temp.std} argument).
  This approach is based on Charles's and Boyle's laws.
  Comparison with calculations using the Peng-Robinson equation of state suggests that error in \code{stdVol} is around 0.1\% for typical biogas with volume measured at 25 degrees C, and higher at higher temperatures (up to 0.3\% at 55 degrees C).

  Standard temperature and pressure and their units can be defined by the user using the \code{temp.std}, \code{pres.std}, \code{temp.unit}, and \code{pres.unit} arguments.  
  Alternatively,  standard values and units of temperature and pressure can be globally set using the function \code{\link{options}}.
  Default values are 0 degrees C and 1.0 atm. 
  \code{stdVol} is vectorized, and if one argument has a shorter length than the others, it will be recycled.

}
\value{
  Standardised gas volume in the same units as \code{vol}.
  A numeric vector.
}
\references{
  Hafner, S.D., Rennuit, C., Triolo, J.M., Richards, B.K. 2015. Validation of a simple gravimetric method for measuring biogas production in laboratory experiments. \emph{Biomass and Bioenergy} \bold{83}, 297-301.

  Richards, B.K., Cummings, R.J., White, T.E., Jewell, W.J. 1991. Methods for kinetic analysis of methane fermentation in high solids biomass digesters. \emph{Biomass and Bioenergy} \bold{1}, 65-73.
}
\author{
  Sasha D. Hafner and Charlotte Rennuit
}

\seealso{
  \code{\link{cumBg}},
  \code{\link{summBg}},
  \code{\link{stdVol}},
  \code{\link{options}}
}
\examples{
  # 100 mL, measured at 35 C
  stdVol(100, temp = 35, pres = 1)

  # Or, with different units
  stdVol(100, temp = 35, pres = 103, unit.pres = "kPa", pres.std = 101.325)
  
  # Vectorized
  data(vol)
  head(vol)
  vol$vol.std <- stdVol(vol$vol, temp = 20, pres = 1.02)
  head(vol)
  
  #using options() (recommended!)
  oldoptions <- options(temp.std = 273.15, pres.std = 101325, 
                        unit.temp = 'K', unit.pres = 'Pa')
  vol$vol.std <- stdVol(vol$vol, temp = 293.15, pres = 101325)
  head(vol)
  options(oldoptions)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\concept{biogas}
