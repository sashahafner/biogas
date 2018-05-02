\name{cumBg}
\alias{cumBg}
%- Also NEED an "\alias" for EACH other topic documented here.
\title{
  Calculate Cumulative Biogas Production
}
\description{
  \code{cumBg} (for \emph{cum}ulative \emph{b}io\emph{g}as) calculates cumulative biogas, methane production and production rates from individual volume (or mass) and composition measurements for any number of reactors.
}
\usage{
cumBg(
  # Main arguments
  dat, dat.type = 'vol', comp = NULL, temp = NULL, 
  pres = NULL, interval = TRUE,
  data.struct = 'long',
  # Column names for volumetric method
  id.name = 'id', time.name = 'time', dat.name = dat.type,
  comp.name = 'xCH4', 
  # Additional arguments for manometric and gravimetric methods
  pres.resid = NULL, temp.init = NULL, pres.init = NULL,
  rh.resid.init = 1, headspace = NULL, 
  vol.hs.name = 'vol.hs', headcomp = 'N2',
  absolute = TRUE, pres.amb = NULL,
  # Additional arguments for GCA method
  mol.f.name = NULL, vol.syr = NULL,
  # Calculation method and other settings
  cmethod = 'removed', imethod = 'linear', extrap = FALSE,
  addt0 = TRUE, showt0 = TRUE, 
  dry = FALSE,
  # Warnings and messages
  std.message = TRUE, check = TRUE,
  # Units and standard conditions
  temp.std = getOption('temp.std', as.numeric(NA)), 
  pres.std = getOption('pres.std', as.numeric(NA)),
  unit.temp = getOption('unit.temp', 'C'),
  unit.pres = getOption('unit.pres', 'atm')
  ) 
}
%- maybe also "usage" for other objects documented here.
\arguments{
  \item{dat}{
    a data frame with reactor identification code; time of measurement (as \code{numeric}, or \code{POSIX}); and measured biogas volume, pressure in \code{pres.unit}, or total reactor mass (see \code{dat.type} argument).
    See Details section for details on units.
    Additional columns can be present--these will be returned in the output data frame.
    See \code{data.struct} argument for details on how data frames are structured.
}
  \item{dat.type}{
  the type of data contained in \code{dat}. 
  Use \code{"vol"} or \code{"volume"} for biogas volume (volumetric method will be used), \code{"mass"} for reactor masses (gravimetric method will be used) or \code{"pres"} (\code{"pressure"}) for headspace pressure (pressure of the headspace will be converted to gas volume).
  Default is \code{"vol"}.
}
  \item{comp}{
    (optional) a data frame with the columns reactor identification code; time of measurement, (as \code{numeric}, or \code{POSIX}); and methane concentration within dry biogas as a mole fraction, considering only methane and carbon dioxide (unless \code{cmethod = "total"}) or a single numeric value.
    If omitted, cumulative biogas volume will still be calculated and returned (but no methane data will be returned).
    The names of these columns are specified with \code{id.name}, \code{time.name}, and \code{comp.name}.
    Default is \code{NULL}.
}
  \item{temp}{
    the temperature at which biogas volume was measured (when \code{dat.type = "vol"}), or of biogas just prior to exiting the reactor (when \code{dat.type = "mass"}).
    A length-one numeric vector.
    Degrees Celcius by default (see \code{unit.temp} argument). 
    Default is \code{NULL}, which suppresses correction for temperature and pressure. 
}
  \item{pres}{
    the absolute pressure at which biogas volume was measured (when \code{dat.type = "vol"}), or of biogas just prior to exiting the reactor (when \code{dat.type = "mass"}).
    A length-one numeric vector or a character vector giving the name of the column in \code{dat} with the pressure measurements.
    Atmospheres by default (see \code{unit.pres} argument). 
    Default is \code{NULL}, which suppresses correction for temperature and pressure. 
    Not used for manometric method (when \code{dat.type = "pres"}).
}

  \item{interval}{
    do biogas measurements (volume or pressure) represent production only from the time interval between observations (default)?
    \code{interval = FALSE} means measured gas volume or pressure is cumulative.
    Applies to volumetric (\code{dat.type = "vol"}) and manometric methods (\code{dat.type = "pres"}). 
    The gravimetric method (\code{dat.type = "mass"}) is cumulative by nature and the \code{interval} argument is not used.
    Default is \code{TRUE}.
  }

  \item{data.struct}{
    the structure of input data. The default of 'long' means separate objects for volume (or pressure or mass for manometric and gravimetric methods) and composition (if available). 
    Each data frame must have reactor identification code and time columns with names specified with \code{id.name} and \code{time.name}.
    The \code{dat} data frame must have volume (or pressure or mass) data in a single column with the name specified by \code{dat.name}.
    The \code{comp} data frame must have biogas composition in a single column with the name specified by \code{comp.name}.
    For the \code{data.struct = 'longcombo'} option, the composition column should be in the \code{dat} data frame, and the \code{comp} argument is not used.
    For the \code{data.struct = 'wide'} option, two separate data frames are needed as in \code{'long'}, but there are no reactor identification code columns.
    Instead, in \code{dat}, volume (or pressure or mass) data are in a separate column for each bottle, and column names are reactor identification codes.
    Here, \code{dat.name} should be the name of the first column with volume (or pressure or mass) data.
    All following columns are assumed to also have volume (or pressure or mass) data.
    And in \code{comp}, biogas composition data are also in a separate column for each bottle, also with reactor identification codes for column names.
    Here, \code{comp.name} should be the name of the first column with biogas composition data, as for \code{dat}.
  }

  \item{id.name}{
    name of the reactor identification code column in \code{dat}. Must be the same in all data frames used in the function. Default is \code{"id"}.
}
  \item{time.name}{
    name of column containing time data in \code{dat} and \code{comp} data frames. Default is \code{"time"}.
}
  \item{dat.name}{
    name of column containing the primary response variable (volume or mass) in \code{dat} data frame. Default is \code{dat.type} value. See \code{dat.type} argument.
}
  \item{comp.name}{
    name of column containing biogas mole fraction of methane in \code{comp} data frame. Default is \code{"xCH4"}. Must be normalised so xCH4 + xCO2 = 1.0 unless \code{cmethod = "total"}.
}

  \item{pres.resid}{
    headspace pressure after venting. 
    Either a single numeric value, assumed to be the same for all observations, or the name of a column in \code{dat} that contains these values for each observation.
    Used in manometric method only (\code{dat.type = "pres"}).
    Can be absolute (default) or gauge depending on the value of \code{absolute}.
    Length one numeric or character vector.
}

  \item{temp.init}{
    initial headspace temperature in \code{unit.temp} units.
    Used to correct for effect of initial reactor headspace on mass loss for the gravimetric method (\code{dat.type = "mass"}), and to determine initial gas volume in the manometric method (\code{dat.type = "pres"}).
    Not used for volumetric method (\code{dat.type = "vol"}).
}

 \item{pres.init}{
    headspace pressure at the begining of the experiment. 
    Used in manometric method only (\code{dat.type = "pres"}).
    Can be absolute (default) or gauge depending on the value of \code{absolute}.
    Length one numeric vector.
}

 \item{rh.resid.init}{
    relative humidity of the gas in the headspace at the begining of the experiment. 
    Typically should be 1.0 (the default).
    Length one numeric vector.
    Used in manometric method only (\code{dat.type = "pres"}).
}

  \item{headspace}{
    (optional) a data frame or length-one numeric vector with reactor headspace volume(s).
    If a data frame is used, it should at least contain a \code{"id"} (reactor identification code) column (see \code{"id.name"}) and headspace volume column (see \code{vol.hs.name} argument).
    Required if \code{method = "total"} for the volumetric method, for initial headspace correction for the gravimetric method (see \code{headcomp} and \code{temp.init}) and for the manometric method using pressure measurements (\code{dat.type = "pres"}).
    Default is \code{NULL}.
}

  \item{vol.hs.name}{
    name of column containing headspace volume data in optional \code{headspace} data frame. 
    Default is \code{"vol.hs"}.
}

  \item{headcomp}{
    (optional) Composition of headspace for correction of initial headspace mass for gravimetric method only.
    If provided, correction will be applied to first mass loss observation for each individual reactor.
    See argument of the same name in \code{\link{mass2vol}}.
    Currently, the only option is \code{"N2"} for dinitrogen gas.
    Use of any other value will be ignored with a warning.
}

 \item{absolute}{
    is the headspace pressure measured in the manometric method (\code{dat.name} and \code{pres.resid} values/columns) absolute or gauge pressure? 
    Applies to manometric method only (\code{dat.type = "pres"}).
    Default is \code{TRUE}. 
  }

 \item{pres.amb}{
    absolute ambient pressure needed to calculate absolute pressure from gauge pressure measurements.
    In atmospheres by default (see \code{unit.pres} argument). 
    Only a single value is accepted. 
    If ambient pressure differed among measurements, it is necessary to convert pressures to absolute values and use \code{absolute = TRUE} instead.
  }

 \item{mol.f.name}{
    name of the column in \code{dat} that contains the post-venting (f for final) quantity of methane in the syringe. 
    Used for the absolute GC method (\code{dat.type = "gca"}) only (Hansen et al., 2004).
    If venting was not done for a particular observation, the value in this column should be NA.
    The pre-venting column name is given in the \code{dat.name} column.
  }

 \item{vol.syr}{
    volume of the syringe used in the absolute GC method (Hansen et al., 2004) (\code{dat.type = "gca"}) in mL.
    Only a single value is accepted. 
    If other volume units are used (e.g., microL), the units used for the (\code{headspace}) argument must match, and the output volume unit will be the same.
  }
 
 \item{cmethod}{
    method for calculating cumulative methane production.
    Applies to \code{dat.type = "vol"} and \code{dat.type = "pres"} methods only. 
    Use \code{"removed"} to base production on \code{xCH4} and gas volumes removed (default).
    Use \code{"total"} to base it on the sum of methane removed and methane remaining in the reactor headspace.
    For \code{"removed"}, \emph{\code{xCH4} should be calculated based on methane and CO2 only (xCH4 + xCO2 = 1.0).}
    For \code{"total"}, \emph{\code{xCH4} should be calculated including all biogas components (CH4, CO2, N2, H2S, etc.) except water.}
    Length one character vector. 
}
  \item{imethod}{
    method used for interpolation of \code{xCH4}.
    This is passed as the \code{method} argument to \code{\link{interp}}.
    Length one character vector. 
    Default is \code{"linear"} for linear interpolation.
}
  \item{extrap}{
    should \code{comp.name} be extrapolated? 
    Length one logical vector.
    This is passed as the \code{extrap} argument to \code{\link{interp}}. 
    Default is \code{FALSE}.
}
 \item{addt0}{
    is the earliest time in \code{dat} data frame \dQuote{time zero} (start time)? 
    If not, this argument adds a row with \code{time.name = 0} for each reactor in order to calculate production rates for the first observation.
    This addition is only made when \code{dat.type = "vol"} or \code{dat.type = "pres"} and \code{time.name} is \code{numeric} (or \code{integer}).
    Length-one logical vector.
    Default is \code{TRUE}.
    To return these additional rows in the output, see \code{showt0}.
}
  \item{showt0}{
    should \dQuote{time zero} rows be returned in the output?
    Can be convenient for plotting cumulative volumes.
    Only applies if \code{time.name} is \code{numeric} (or \code{integer}).
    These rows may have been present in the original data (\code{dat}) or added by the function (see \code{addt0}).
    Default value depends on \code{dat} \code{time.name} column content. 
    If \code{time.name} column is numeric and contains 0 then the default value is \code{TRUE} and otherwise \code{FALSE}. 
}
 
  \item{dry}{
    set to \code{TRUE} is volume data are standardised to dry conditions (e.g., AMPTS II data). 
    Only applies to volumetric data (dat.type = 'vol').
    The default (\code{FALSE}) means biogas is assumed to be saturated with water vapor.
  }

  \item{std.message}{
    should a message with the standard conditions be displayed? 
    Default is \code{TRUE}. 
  }
  
  \item{check}{
    should input data be checked for unreasonable values (with warnings)? 
    Currently only composition values are checked.
    Default is \code{TRUE}.
  }
 
 
  \item{temp.std}{
    standard temperature for presentation of biogas and methane results.
    Length one numeric vector.
    Default value is 0 degrees C (set in \code{\link{stdVol}}).
    Argument is passed to \code{\link{stdVol}}.
}
  \item{pres.std}{
    standard pressure for presentation of biogas and methane results.
    Length one numeric vector.
    Default value is 1.0 atm (101325 Pa) (set in \code{\link{stdVol}}).
    Argument is passed to \code{\link{stdVol}}.
}
   \item{unit.temp}{
    temperature units for \code{temp} and \code{temp.std} arguments.
    %Options are \code{"C"} (degrees Celcius), \code{"F"} (degrees Fahrenheit), and \code{"K"} (Kelvin). 
    Default is \code{"C"}. 
    Argument is passed to \code{\link{stdVol}}.
}
    \item{unit.pres}{
    pressure units for \code{pres} and \code{pres.std} arguments.
    %Options are \code{"atm"}, \code{"Pa"}, \code{"kPa"}, \code{"hPa"}, and \code{"bar"}. 
    Default is \code{"atm"}.
    Argument is passed to \code{\link{stdVol}}.
}

}
\details{
  Using volume, mass, pressure, or molar quantity data from \code{dat} and gas composition from \code{comp}, this function will calculate standardised biogas and methane production (if \code{comp} is provided) for each observation,  interpolating \code{comp.name} (from \code{comp} argument) to each \code{time.name} in \code{dat} if needed, and summing these for cumulative values.
  Use of volumetric measurements is the default case. 
  Alternatively, if reactor mass is given in \code{dat}, a gravimetric approach (Hafner et al. 2015) can be applied by setting \code{dat.type} to \code{"mass"}.
  In this case, to determine total cumulative biogas production for batch reactors, an initial mass should be included for each reactor.
  If pressure is measured in \code{dat}, a manometric method can be applied by setting \code{dat.type} to \code{"pres"}.  
  Lastly, molar quantity of methane can be used in the absolute GC method by setting \code{dat.type} to \code{"gca"}.  

  For the volumetric and absolute GC methods, all volumes (input and output) have the same units, e.g., mL, L, SCF.
  For the gravimetric method, output volumes will be in mL if input masses are in g (or in L if input masses are in kg, etc.).
  For the manometric method, inputs units are specified with \code{unit.pres}, and output volumes are the same as headspace volume, e.g., mL or L.
  
  Biogas composition (\code{comp.name} column in \code{comp}) is specified as the mole fraction of methane in dry biogas, normalised so mole fractions of methane and carbon dioxide sum to unity (Richards et al. 1991). Alternatively, if \code{cmethod} is set to \code{"total"}, biogas composition is the mole fraction of methane in dry biogas (include all the other gases except water). 
 
  Standard values and units for temperature and pressure can be globally set using the function \code{\link{options}}.
  See \code{\link{stdVol}}.
  To surpress volume correction to a \dQuote{standard} temperature and pressure, leave \code{temp} as \code{NULL} (the default).

  If \code{check = TRUE}, the input values of mole fraction of methane in biogas are checked, and a warning is returned if the are outside 0, 1.

  
}

\value{
  a data frame with all the columns originally present in \code{dat}, plus these others:
  \item{vBg}{Standardised volume of biogas production for individual event.}
  \item{xCH4}{Interpolated mole fraction of methane in biogas.}
  \item{vCH4}{Standardised volume of methane production for individual event. Only if \code{comp} is provided.}
  \item{vhsCH4}{Standardised volume of methane present in reactor headspace. Only if \code{method = "total"} is used.}
  \item{cvBg}{Standardised cumulative volume of biogas production.}
  \item{cvCH4}{Standardised cumulative volume of methane production. Only if \code{comp} is provided.}
  \item{rvBg}{Production rate of biogas.}
  \item{rvCH4}{Production rate of methane. Only if \code{comp} is provided.}
}
\references{
  Hafner, S.D., Rennuit, C., Triolo, J.M., Richards, B.K. 2015. Validation of a simple gravimetric method for measuring biogas production in laboratory experiments. \emph{Biomass and Bioenergy} \bold{83}, 297-301.

  Hansen, T.L., Schmidt, J.E., Angelidaki, I., Marca, E., Jansen, J. la C., Mosbak, H. and Christensen, T.H. 2004. Method for determination of methane potentials of solid organic waste. \emph{Waste Management} \bold{24}, 393-400

  Richards, B.K., Cummings, R.J., White, T.E., Jewell, W.J. 1991. Methods for kinetic analysis of methane fermentation in high solids biomass digesters. \emph{Biomass and Bioenergy} 1: 65-73.
}
\author{
  Sasha D. Hafner and Charlotte Rennuit
}

\seealso{
  \code{\link{summBg}},
  \code{\link{interp}},
  \code{\link{stdVol}},
  \code{\link{options}}
}

\examples{

data("vol")
data("comp")

head(vol)
head(comp)

# Calculate cumulative production and rates from vol and comp
cum.prod <- cumBg(vol, comp = comp, temp = 20, pres = 1, id.name = "id", time.name = "days", 
		  comp.name = "xCH4", dat.name = "vol")
head(cum.prod)

# Note warnings and related NAs in results
# Set extrap = TRUE to extrapolate xCH4 to earliest times

# Calculate cumulative production and rates from vol and comp
cum.prod <- cumBg(vol, comp = comp, temp = 20, pres = 1, id.name = "id", time.name = "days", 
		  comp.name = "xCH4", dat.name = "vol", extrap = TRUE)
head(cum.prod)

# In this case, we can use default values for some column names, so this call is identical
cum.prod <- cumBg(vol, comp = comp, temp = 20, pres = 1, time.name = "days", extrap = TRUE)

# Plot results
\dontrun{
# Not run just because it is a bit slow
library(ggplot2)
qplot(x = days, y = cvCH4, data = cum.prod, xlab = "Time (d)", 
      ylab = "Cumulative methane production (mL)",color = id, geom = "line")
}

# Omit added time zero rows
cum.prod <- cumBg(vol, comp = comp, temp = 20, pres = 1, time.name = "days", extrap = TRUE, 
		  showt0 = FALSE)
head(cum.prod)

\dontrun{
# Not run just because it is a bit slow
qplot(x = days, y = cvCH4, data = cum.prod, xlab = "Time (d)", 
      ylab = "Cumulative methane production (mL)", color = id, geom = "line")
}

# Previous is different from never adding them in the first place (rates not calculated for first 
# observations here)
cum.prod <- cumBg(vol, comp = comp, temp = 20, pres = 1, time.name = "days", extrap = TRUE, 
		  addt0 = FALSE)
head(cum.prod)

\dontrun{
# Not run just because it is a bit slow
qplot(x = days, y = cvCH4, data = cum.prod, xlab = "Time (d)", 
      ylab = "Cumulative methane production (mL)", color = id, geom = "line")
}

# Can use POSIX objects for time (but cumBg cannot add t0 rows here)
class(vol$date.time)
class(comp$date.time)
cum.prod <- cumBg(vol, comp = comp, temp = 20, pres = 1, time.name = "date.time", extrap = TRUE)
head(cum.prod)

\dontrun{
# Not run just because it is a bit slow
qplot(x = date.time, y = cvCH4, data = cum.prod, xlab = "Time (d)", 
      ylab = "Cumulative methane production (mL)", color = id, geom = "line")
}

# Can leave out composition data, and then CH4 is not included in results
cum.prod <- cumBg(vol, temp = 20, pres = 1, time.name = "days")
head(cum.prod)

# Leave out pres or temp, and results are not standardised
cum.prod <- cumBg(vol, time.name = "days")
head(cum.prod)

# Example with input data frames with different column names
data("vol2")
data("comp2")

head(vol2)
head(comp2)

cum.prod <- cumBg(vol2, comp = comp2, temp = 20, pres = 1, 
                  id.name = "bottle", time.name = "days", 
		  dat.name = "meas.vol", comp.name = "CH4.conc")
head(cum.prod)
tail(cum.prod)

# Note warnings and related NAs in results
warnings()

# Set extrap = TRUE to avoid
cum.prod <- cumBg(vol2, comp = comp2, temp = 20, pres = 1, 
                  id.name = "bottle", time.name = "days", 
		  dat.name = "meas.vol", comp.name = "CH4.conc", 
                  extrap = TRUE)
head(cum.prod)

\dontrun{
# Not run just because it is a bit slow
qplot(x = days, y = cvCH4, data = cum.prod, xlab = "Time (d)", 
      ylab = "Cumulative methane production (mL)", 
      color = bottle, geom = "line")
}

# Mass example
data("mass")

mass

# Need to specify data type with dat.type argument (using default 
# values for id.name, dat.name, and comp.name)
cum.prod <- cumBg(mass, dat.type = "mass", comp = comp, temp = 35, 
                  pres = 1, time.name = "days")
cum.prod

# Drop time 0 rows
cum.prod <- cumBg(mass, dat.type = "mass", comp = comp, temp = 35, 
                  pres = 1, time.name = "days", showt0 = FALSE)
cum.prod

# Add initial headspace correction (alternatively, headspace could 
# be a data frame with a different volume for each reactor)
cum.prod <- cumBg(mass, dat.type = "mass", comp = comp, temp = 35, 
                  pres = 1, time.name = "days", headspace = 300, 
                  headcomp = "N2", temp.init = 20, showt0 = FALSE)
cum.prod

# Pressure example
data("strawPressure")
data("strawComp")
data("strawSetup")

cum.prod <- cumBg(strawPressure, dat.type = 'pres', 
                  comp = strawComp, temp = 35, 
                  id.name = 'bottle', time.name ='time', 
                  dat.name = 'pres', comp.name = 'xCH4',
                  pres.resid = 'pres.resid', temp.init = 20, 
                  pres.init = 101.325,
                  headspace = strawSetup, 
                  vol.hs.name = 'headspace', 
                  extrap = TRUE, 
                  unit.pres = 'kPa', pres.std = 101.325)

head(cum.prod)

# Absolute GC method (long format is the only option)

# Generate some data--two bottles with identical results
# Units for n1 and n2 are micromoles of CH4
biogas <- data.frame(id = rep(c('A', 'B'), each = 5), 
                     time.d = rep(1:5, 2), 
                     n1 = rep(c(1.1, 2.4, 3.8, 5.9, 2.3), 2), 
                     n2 = rep(c(NA, NA, NA, 1.2, NA), 2))

# Bottles were vented after the measurements on day 4
biogas

# Syringe volume is 0.25 mL
# Headspace volume is 50 mL

cp <- cumBg(biogas, id.name = 'id', time.name = 'time.d', 
            dat.name = 'n1', mol.f.name = 'n2', vol.syr= 0.25,
            headspace = 50, dat.type = 'gca')
cp

# Suppose the bottles had two difference headspace volumes
setup <- data.frame(id = c('A', 'B'), vol.hs = c(50, 60))

cp2 <- cumBg(biogas, id.name = 'id', time.name = 'time.d', 
            dat.name = 'n1', mol.f.name = 'n2', vol.syr= 0.25,
            headspace = setup, vol.hs.name = 'vol.hs', dat.type = 'gca')
cp2


# Different data structures
# Load example data
data("s3voll")
data("s3volw")
data("s3compl")
data("s3compw")
data("s3lcombo")

# wide

# Check data structure first
s3volw

s3compw

cum.prod <- cumBg(s3volw, comp = s3compw, temp = 25, pres = 1,
                  time.name = 'time.d',
                  data.struct = 'wide',
                  dat.name = 'D', comp.name = 'D',
                  extrap = TRUE)

cum.prod

# longcombo
s3lcombo

cum.prod <- cumBg(s3lcombo, temp = 25, pres = 1,
                  id.name = 'id', time.name = 'time.d',
                  data.struct = 'longcombo',
                  dat.name = 'vol.ml', comp.name = 'xCH4',
                  extrap = TRUE)

# Compare wide and longcombo to long (default)
s3voll
s3compl

cum.prod <- cumBg(s3lcombo, comp = s3compl, temp = 25, pres = 1,
                  id.name = 'id', time.name = 'time.d',
                  dat.name = 'vol.ml', comp.name = 'xCH4',
                  extrap = TRUE)

cum.prod


}
%% Add one or more standard keywords, see file "KEYWORDS" in the
%% R documentation directory.
\keyword{chron}
\keyword{manip}
\concept{biogas}
