\name{mass2vol}
\alias{mass2vol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calculate Biogas Production Gravimetrically
}
\description{
  \code{mass2vol} calculates biogas volume based on measured reactor mass loss.
}
\usage{
mass2vol(mass, xCH4, temp, pres, 
  temp.std = getOption('temp.std', 0.0), 
  pres.std = getOption('pres.std', 1.0), 
  unit.temp = getOption('unit.temp', 'C'), 
  unit.pres = getOption('unit.pres', 'atm'), 
  value = "CH4", headspace = NULL, headcomp = 'N2', 
  temp.init = NULL, std.message = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mass}{
    reactor mass loss in g. A numeric vector.
}
  \item{xCH4}{
    biogas mole fraction of methane. Must be normalised so xCH4 + xCO2 = 1.0. A numeric vector.
}
  \item{temp}{
    the temperature of biogas as it exited the reactor. A length-one numeric vector.
    Degrees Celcius by default (see \code{unit.temp} argument).
}
  \item{pres}{
    the pressure of biogas as it exited the reactor. A length-one numeric vector.
    Atmospheres (atm) by default (see \code{unit.pres} argument).
}

% \item{rh}{
%    relative humidity of the gas (\code{0<=rh<=1}).
%    Typically should be 1.0 (the default).
%    A numeric vector.
%}
  \item{temp.std}{
    standard temperature for presentation of biogas and methane results.
    Default value is 0 degrees C.
    Argument is passed to \code{\link{stdVol}}.
    Length one numeric vector.
}
  \item{pres.std}{
    standard pressure for presentation of biogas and methane results.
    Default value is 1.0 atm.
    Argument is passed to \code{\link{stdVol}}.
    Length one numeric vector.
}
  \item{unit.temp}{
    temperature units for \code{temp} and \code{temp.std} arguments.
    Default is \code{"C"}.
    Argument is passed to \code{\link{stdVol}}.
}
  \item{unit.pres}{
    pressure units for \code{pres} and \code{pres.std} arguments.
    Default is \code{"atm"}.
    Argument is passed to \code{\link{stdVol}}.
}
  \item{value}{
    what should be returned?
    Default is \code{"CH4"}, which returns methane volume in mL.
    \code{"all"} returns biogas, methane, and carbon dioxide volumes.
}
  \item{headspace}{
    (optional) reactor headspace volume in mL. 
    Used to correct for effect of initial reactor headspace on mass loss.
    A numeric vector.
}
  \item{headcomp}{
    (optional) composition of the initial reactor headspace.
    Only required if \code{headspace} argument is specified.
    Used to correct for effect of initial reactor headspace on mass loss.
    Currently, the only option is \code{"N2"} for dinitrogen gas.
    Use of any other value will be ignored with a warning.
}
  \item{temp.init}{
    (optional) initial headspace temperature in unit.temp units.
    Used to correct for effect of initial reactor headspace on mass loss.
}

 \item{std.message}{should a message with the standard conditions be displayed? 
   Default is TRUE. 
}
}
\details{
  This function uses the method described in Hafner et al. (2015) to calculate biogas production from reactor mass loss.
  It is essential that the only change in reactor mass is due to biogas removal!
  Users are advised to read the original reference before applying the method.
  This function is vectorized.
  Instead of using this function directly when working with multiple measurements on multiple reactors, use the \code{\link{cumBg}} function (which can call up \code{mass2vol}). 
  
  Standard values and units of temperature and pressure can be globally set using the function \code{\link{options}}.
}
\value{
volume of methane in mL as a numeric vector (if \code{value = "CH4"}) or a matrix with columns for biogas, methane, and carbon dioxide volumes in mL.
  \item{vBg}{standardised volume of biogas in mL}
  \item{vCH4}{standardised volume of methane in mL} 
  \item{vCH4}{standardised volume of carbon dioxyde in mL} 
  All volumes are standardised using \code{pres.std} and \code{temp.std} arguments (see \code{\link{stdVol}}).
}
\references{
  Hafner, S.D., Rennuit, C., Triolo, J.M., Richards, B.K. 2015. Validation of a simple gravimetric method for measuring biogas production in laboratory experiments. \emph{Biomass and Bioenergy} \bold{83}, 297-301.
}
\author{
  Sasha D. Hafner and Charlotte Rennuit
}

\seealso{
  \code{\link{vol2mass}},
  \code{\link{stdVol}},
  \code{\link{cumBg}},
  \code{\link{options}}
}
\examples{
  # Volume of methane if measured mass loss was 3.1 g
  mass2vol(3.1, xCH4 = 0.65, temp = 35, pres = 1)

  # More details
  mass2vol(3.1, xCH4 = 0.65, temp = 35, pres = 1, value = "all")
  
  # Vectorized
  data("massw")
  massw$massloss <- massw$start - massw$end
  massw$vCH4 <- mass2vol(massw$massloss, xCH4 = 0.65, temp = 35, 
                         pres = 1)
  massw
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\concept{biogas}
