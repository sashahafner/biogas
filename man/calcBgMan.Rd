\name{calcBgMan}
\alias{calcBgMan}
%- Also NEED an "\alias" for EACH other topic documented here.
\title{
  Calculate Cumulative Biogas Production from Pressure Data
}
\description{
  \code{calcBgMan} (for \emph{cum}ulative \emph{b}io\emph{g}as \emph{man}ometric) calculates cumulative biogas, methane production and production rates from individual pressure and composition measurements for any number of reactors.
}
\usage{
calcBgMan(
  # Main arguments
  dat, comp = NULL, temp, interval = TRUE,
  data.struct = 'longcombo',
  # Column names 
  id.name = 'id', time.name = 'time',
  pres.name = 'pres', comp.name = NULL,
  # Additional arguments
  pres.resid = NULL, temp.init = NULL, pres.init = NULL,
  rh.resid = NULL,
  rh.resid.init = 1, headspace = NULL, 
  vol.hs.name = 'vol.hs',
  absolute = TRUE, pres.amb = NULL,
  # Calculation method and other settings
  cmethod = 'removed', imethod = 'linear', extrap = FALSE,
  addt0 = TRUE, showt0 = TRUE, 
  # Warnings and messages
  std.message = !quiet, 
  check = TRUE,
  # Units and standard conditions
  temp.std = getOption('temp.std', as.numeric(NA)), 
  pres.std = getOption('pres.std', as.numeric(NA)),
  unit.temp = getOption('unit.temp', 'C'),
  unit.pres = getOption('unit.pres', 'atm'),
  quiet = FALSE
  ) 
}
%- maybe also "usage" for other objects documented here.
\arguments{
  \item{dat}{
    a data frame with reactor identification code; time of measurement (as \code{numeric}, or \code{POSIX}); and measured pressure in \code{pres.unit}.
    See Details section for details on units.
    Additional columns can be present--these will be returned in the output data frame.
    See \code{data.struct} argument for details on how data frames are structured.
}
  \item{comp}{
    (optional) a data frame with the columns reactor identification code; time of measurement, (as \code{numeric}, or \code{POSIX}); and methane concentration within dry biogas as a mole fraction, considering only methane and carbon dioxide (unless \code{cmethod = "total"}) or a single numeric value.
    If omitted, cumulative biogas volume will still be calculated and returned (but no methane data will be returned).
    The names of these columns are specified with \code{id.name}, \code{time.name}, and \code{comp.name}.
    Default is \code{NULL}.
}
  \item{temp}{
    the temperature at which headspace pressure was measured.
    A length-one numeric vector or length-one character vector referring to a column in \code{dat}.
    Degrees Celcius by default (see \code{unit.temp} argument). 
}

  \item{interval}{
    do biogas pressure measurements represent production only from the time interval between observations (default)?
    \code{interval = FALSE} means measured pressure is cumulative.
    For mixed manometric measurements (some observations vented, some not), use \code{interval = TRUE}, include a column for \code{pres.resid}, and, if composition was only measured for vented observations, use \code{imethod = "f1"}.
    Default is \code{TRUE}.
  }

  \item{data.struct}{
    the structure of input data. The default of 'longcombo' means headspace pressure and composition in a single column.
    The \code{dat} data frame must have reactor identification code and time columns with names specified with \code{id.name} and \code{time.name}, volume data in a single column with the name specified by \code{pres.name}, and biogas composition in a single column with the name specified by \code{comp.name}
    For the \code{data.struct = 'long'} option, two separate data frames are needed, one with volume and one with composition.
    Each data frame must have reactor identification code and time columns with names specified with \code{id.name} and \code{time.name}.
    The \code{dat} data frame must have volume data in a single column with the name specified by \code{pres.name}.
    The \code{comp} data frame must have biogas composition in a single column with the name specified by \code{comp.name}.
    For the \code{data.struct = 'wide'} option, two separate data frames are needed as in \code{'long'}, but there are no reactor identification code columns.
    Instead, in \code{dat}, volume data are in a separate column for each bottle, and column names are reactor identification codes.
    Here, \code{pres.name} should be the name of the first column with volume data.
    All following columns are assumed to also have volume data.
    And in \code{comp}, biogas composition data are also in a separate column for each bottle, also with reactor identification codes for column names.
    Here, \code{comp.name} should be the name of the first column with biogas composition data, as for \code{dat}.
  }

  \item{id.name}{
    name of the reactor identification code column in \code{dat}. Must be the same in all data frames used in the function. Default is \code{"id"}.
}
  \item{time.name}{
    name of column containing time data in \code{dat} and \code{comp} data frames. Default is \code{"time"}.
}
  \item{pres.name}{
    name of column containing the primary response variable (pressure) in \code{dat} data frame. Default is \code{dat.type} value. See \code{dat.type} argument.
}
  \item{comp.name}{
    name of column containing biogas mole fraction of methane in \code{comp} data frame. Default is \code{"xCH4"}. Must be normalised so xCH4 + xCO2 = 1.0 unless \code{cmethod = "total"}.
}

  \item{pres.resid}{
    headspace pressure after venting. 
    Either a single numeric value, assumed to be the same for all observations, or the name of a column in \code{dat} that contains these values for each observation.
    Can be absolute (default) or gauge depending on the value of \code{absolute}.
    Length one numeric or character vector.
}

  \item{temp.init}{
    initial headspace temperature in \code{unit.temp} units.
    Used to determine initial gas volume.
}

 \item{pres.init}{
    headspace pressure at the begining of the experiment. 
    Can be absolute (default) or gauge depending on the value of \code{absolute}.
    Length one numeric vector.
}

 \item{rh.resid}{
    relative humidity of the gas in the headspace after venting.
    If NULL (default), it is calculated for each interval as the ratio of post- to pre-venting pressure (equivalent to assuming relative water vapor concentration (mixing ratio) is the same before and after venting.
    If entered, typically should be 1.0.
    Length one numeric vector.
}

 \item{rh.resid.init}{
    relative humidity of the gas in the headspace at the begining of the experiment. 
    Typically should be 1.0 (the default).
    Length one numeric vector.
}

  \item{headspace}{
    (optional) a data frame or length-one numeric vector with reactor headspace volume(s).
    If a data frame is used, it should at least contain a \code{"id"} (reactor identification code) column (see \code{"id.name"}) and headspace volume column (see \code{vol.hs.name} argument).
    Required if \code{cmethod = "total"}.
    Default is \code{NULL}.
}

  \item{vol.hs.name}{
    name of column containing headspace volume data in optional \code{headspace} data frame. 
    Default is \code{"vol.hs"}.
}

 \item{absolute}{
    is the headspace pressure measured (\code{pres.name} and \code{pres.resid} values/columns) absolute or gauge pressure? 
    Default is \code{TRUE}. 
  }

 \item{pres.amb}{
    absolute ambient pressure needed to calculate absolute pressure from gauge pressure measurements.
    In atmospheres by default (see \code{unit.pres} argument). 
    Only a single value is accepted. 
    If ambient pressure differed among measurements, it is necessary to convert pressures to absolute values and use \code{absolute = TRUE} instead.
  }
 
 \item{cmethod}{
    method for calculating cumulative methane production.
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
    This addition is only made when \code{time.name} is \code{numeric} (or \code{integer}).
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

  \item{std.message}{
    should a message with the standard conditions be displayed? 
    Default is \code{TRUE}. 
  }
  
  \item{check}{
    should input data be checked for unreasonable values (with warnings)? 
    Currently only composition values are checked.
    Default is \code{TRUE}.
    Values are changed if outside 0, 1 (divided by 100).
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
  \item{quiet}{
    use to suppress messages. Default is \code{FALSE}.
}

}
\details{
  Using pressure data from \code{dat} and gas composition from \code{comp}, this function will calculate standardised biogas and methane production (if \code{comp} is provided) for each observation,  interpolating \code{comp.name} (from \code{comp} argument) to each \code{time.name} in \code{dat} if needed, and summing these for cumulative values.
  Inputs units are specified with \code{unit.pres}, and output volumes are the same as headspace volume, e.g., mL or L.
  
  Biogas composition (\code{comp.name} column in \code{comp}) is specified as the mole fraction of methane in dry biogas, normalised so mole fractions of methane and carbon dioxide sum to unity (Richards et al. 1991). Alternatively, if \code{cmethod} is set to \code{"total"}, biogas composition is the mole fraction of methane in dry biogas (include all the other gases except water). 
 
  Standard values and units for temperature and pressure can be globally set using the function \code{\link{options}}.
  See \code{\link{stdVol}}.
  To surpress volume correction to a \dQuote{standard} temperature and pressure, leave \code{temp} as \code{NULL} (the default).

  If \code{check = TRUE}, the input values of mole fraction of methane in biogas are checked, and a warning is returned if the are outside 0, 1.

  See associated vignette (\code{calcBgMan_function.Rnw}) for more information.
  
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
  Sasha D. Hafner and Nanna Lojborg
}

\seealso{
  \code{\link{cumBg}},
  \code{\link{calcBgVol}},
  \code{\link{calcBgGD}},
  \code{\link{summBg}},
  \code{\link{interp}},
  \code{\link{stdVol}},
  \code{\link{options}}
}

\examples{

# Example with longcombo structured input data frame
data("sludgeTwoBiogas")
data("sludgeTwoSetup")


head(sludgeTwoBiogas)
head(sludgeTwoSetup)

# Calculate cumulative production and rates 
# Pressure is gauge (not absolute) so absolute argument needed
# Data structure is default of longcombo
cbg <- calcBgMan(sludgeTwoBiogas, temp = 30,
                 id.name = "id", time.name = "time.d", 
                 pres.name = "pres", comp.name = "xCH4n",
                 temp.init = 30, pres.resid = 0, pres.init = 0,
                 headspace = sludgeTwoSetup, vol.hs.name = "vol.hs",
                 pres.amb = 1013, absolute = FALSE,
                 unit.pres = "mbar")
head(cbg)

# Plot results
\dontrun{
  # Not run just because it is a bit slow
  library(ggplot2)

  ggplot(cbg, aes(time.d, cvCH4, colour = factor(id))) + 
    geom_point() +
    geom_line(aes(group = id)) +
    labs(x = "Time (d)", y = "Cumulative methane production  (mL)", colour = "Bottle id")  + 
    theme_bw() 
}

# This sludgeTwoBiogas dataset has original xCH4 as well as normalized values
# So "method 2" can also be used by changing comp.name and cmethod arguments
cbg2 <- calcBgMan(sludgeTwoBiogas, temp = 30,
                  id.name = "id", time.name = "time.d", 
                  pres.name = "pres", comp.name = "xCH4",
                  temp.init = 30, pres.resid = 0, pres.init = 0,
                  headspace = sludgeTwoSetup, vol.hs.name = "vol.hs",
                  pres.amb = 1013, cmethod = 'total', absolute = FALSE,
                  unit.pres = "mbar")
head(cbg2)

# Compare
quantile(cbg2$vCH4 - cbg$vCH4)
# Median difference of 0.2 mL

# Example with long structured input data frame
data("strawPressure")
data("strawComp")
data("strawSetup")

# Need to specify data structure with \code{data.struct} argument
# Using default values for time.name, pres.name
cbg <- calcBgMan(strawPressure, comp = strawComp, temp = 31,
                 data.struct = "long",
                 id.name = "bottle", comp.name = "xCH4",
                 temp.init = 21.55, pres.resid = "pres.resid", pres.init = 0,
                 headspace = strawSetup, vol.hs.name = "headspace",
                 pres.amb = 101.3, absolute = FALSE,
                 unit.pres = "kPa")

# Because of missing composition measurements at last time for some bottles
# CH4 volume will be missing
# Can estimate xCH4 here by extrapolation using argument of same name

cbg2 <- calcBgMan(strawPressure, comp = strawComp, temp = 31,
                 data.struct = "long",
                 id.name = "bottle", comp.name = "xCH4",
                 temp.init = 21.55, pres.resid = "pres.resid", pres.init = 0,
                 headspace = strawSetup, vol.hs.name = "headspace",
                 pres.amb = 101.3, absolute = FALSE, 
                 extrap = TRUE, unit.pres = "kPa")


# For example with wide structured input data frame calcBgVol() help file

}
%% Add one or more standard keywords, see file "KEYWORDS" in the
%% R documentation directory.
\keyword{chron}
\keyword{manip}
\concept{biogas}
