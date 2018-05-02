\name{vol2mol}
\alias{vol2mol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calculate Moles of a Gas
}
\description{
  \code{vol2mol} calculates the moles of a gas for a measured volume, temperature, and pressure.
}
\usage{
vol2mol(vol, gas = "CH4", temp, pres, rh = 0, 
	unit.temp = getOption('unit.temp', 'C'), 
	unit.pres = getOption('unit.pres', 'atm'), 
	tp.message = TRUE)
}

\arguments{
  \item{vol}{
  measured gas volume in mL.
  Numeric vector.
}
  \item{gas}{
  chemical formula of gas.
  Currently options are \code{c("CH4", "CO2", "N2", "H2")}.
  Character vector.
}
  \item{temp}{
    temperature of gas at time of volume measurement.
    Numeric vector.
}  
  \item{pres}{
    pressure of gas at time of volume measurement, in the units specified in \code{unit.pres} (default of atm).
    Numeric vector.
}
  \item{rh}{
    relative humidity of the gas at time of measurement.
    Length one numeric vector between zero and 1.0.
    Default is zero (dry gas).
  }
  \item{unit.temp}{
    temperature units.
    Options are \code{"C"} (degrees Celcius, the default), \code{"F"}, and \code{"K"}.
    Length-one character vector.
}
   \item{unit.pres}{
    pressure units.
    Options are \code{"atm"} (the default), \code{"Pa"}, \code{"kPa"}, \code{"hPa"}, and \code{"bar"}.
    Length-one character vector.
}
  \item{tp.message}{
    should a message display \code{temp} and \code{pres} to avoid unit mistakes?
    Length one logical vector.
  }
}
\details{
   This function uses a simple and approximate approach for the conversion, based on Charles's and Boyle's laws, with NIST values for 0 degrees C and 1.0 atm taken as the reference state (Lemmon et al. 2011).
   Measured volume is normalized to 1.0 atm and 0 degrees C using \code{\link{stdVol}}, and the result is divided by the molar volume at the same conditions, as reported by NIST.
   Resulting error should be within 0.5\% for temperature and pressure close to ambient, and is usually below 0.2\%.
}
\value{
  gas amount in moles as a numeric vector.
}
\references{
Lemmon EW, McLinden MO, Friend DG. Thermophysical Properties of Fluid Systems. In Linstrom PJ, Mallard WG, editors. \emph{Chemistry WebBook}, NIST Standard Reference Database Number 69. Gaithersburg, MD: National Institute of Standards and Technology, 2011.
}
\author{
  Sasha D. Hafner
}

\seealso{
  \code{\link{vol2mass}},
  \code{\link{mass2vol}},
  \code{\link{options}}
}
\examples{
# Assume we have measured 253 mL CH4 at 1.0 atm and 22 C, dry
vol2mol(253, "CH4", temp = 22, pres = 1)

# Compare to results to NIST values for the least ideal gas
# Results are within 0.5%
# Expect 0.04108 mol
vol2mol(1000, "CO2", temp = 25, pres = 1)

# Expect 0.08258 mol
vol2mol(1000, "CO2", temp = 25, pres = 2)

# Expect 0.07482 mol
vol2mol(1000, "CO2", temp = 55, pres = 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\concept{biogas}
