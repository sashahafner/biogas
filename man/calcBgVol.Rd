\name{calcBgVol}
\alias{calcBgVol}
%- Also NEED an "\alias" for EACH other topic documented here.
\title{
  Calculate Cumulative Biogas Production from Volumetric Data
}
\description{
  \code{calcBgVol} (for \emph{calc}ulation of \emph{b}io\emph{g}as production from \emph{vol}umetric) measurements) calculates cumulative biogas, methane production and production rates from individual volume and composition measurements for any number of bottles.
}
\usage{
calcBgVol(
  # Main arguments
  dat, comp = NULL, temp = NULL,
  pres = NULL, interval = TRUE, 
  data.struct = 'longcombo',
  # Column names
  id.name = 'id', time.name = 'time', vol.name = 'vol',
  comp.name = NULL,
  # Additional arguments
  headspace = NULL, vol.hs.name = 'vol.hs',
  # Calculation method and other settings
  cmethod = 'removed', imethod = 'linear', extrap = FALSE, 
  addt0 = TRUE, showt0 = TRUE,
  dry = FALSE, 
  empty.name = NULL,
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
    a data frame with bottle identification code; time of measurement (as \code{numeric}, or \code{POSIX}); and measured biogas volume.
    See Details section for details on units.
    Additional columns can be present--these will be returned in the output data frame.
    See \code{data.struct} argument for details on how data frames are structured.
}

  \item{comp}{
    (optional) a data frame with the columns bottle identification code; time of measurement, (as \code{numeric}, or \code{POSIX}); and methane concentration within dry biogas as a mole fraction, considering only methane and carbon dioxide (unless \code{cmethod = "total"}) or a single numeric value.
    If omitted, cumulative biogas volume will still be calculated and returned (but no methane data will be returned).
    The names of these columns are specified with \code{id.name}, \code{time.name}, and \code{comp.name}.
    Default is \code{NULL}.
}
  \item{temp}{
    the temperature at which biogas volume was measured.
    A length-one numeric vector.
    Degrees Celcius by default (see \code{unit.temp} argument). 
    Default is \code{NULL}, which suppresses correction for temperature and pressure. 
}
  \item{pres}{
    the absolute pressure at which biogas volume was measured.
    A length-one numeric vector or a character vector giving the name of the column in \code{dat} with the pressure measurements.
    Atmospheres by default (see \code{unit.pres} argument). 
    Default is \code{NULL}, which suppresses correction for temperature and pressure. 
}

  \item{interval}{
    do biogas volume measurements represent production only from the time interval between observations (default)?
    \code{interval = FALSE} means measured gas volume is cumulative.
    Default is \code{TRUE}.
  }

  \item{data.struct}{
    the structure of input data. The default of 'longcombo' means separate objects for volume and composition (if available).
    The \code{dat} data frame must have bottle identification code and time columns with names specified with \code{id.name} and \code{time.name}, volume data in a single column with the name specified by \code{vol.name}, and biogas composition in a single column with the name specified by \code{comp.name}
    For the \code{data.struct = 'long'} option, two separate data frames are needed, one for volume and one for composition (if available).
    Each data frame must have bottle identification code and time columns with names specified with \code{id.name} and \code{time.name}.
    The \code{dat} data frame must have volume data in a single column with the name specified by \code{vol.name}.
    The \code{comp} data frame must have biogas composition in a single column with the name specified by \code{comp.name}.
    For the \code{data.struct = 'wide'} option, two separate data frames are needed as in \code{'long'}, but there are no bottle identification code columns.
    Instead, in \code{dat}, volume data are in a separate column for each bottle, and column names are bottle identification codes.
    Here, \code{vol.name} should be the name of the first column with volume data.
    All following columns are assumed to also have volume data.
    And in \code{comp}, biogas composition data are also in a separate column for each bottle, also with bottle identification codes for column names.
    Here, \code{comp.name} should be the name of the first column with biogas composition data, as for \code{dat}.
  }

  \item{id.name}{
    name of the bottle identification code column in \code{dat}. Must be the same in all data frames used in the function. Default is \code{"id"}.
}
  \item{time.name}{
    name of column containing time data in \code{dat} and \code{comp} data frames. Default is \code{"time"}.
}
  \item{vol.name}{
    name of column containing the primary response variable (as-measured volume) in \code{dat} data frame. Default is \code{vol}.
}
  \item{comp.name}{
    name of column containing biogas mole fraction of methane in \code{comp} data frame. Default is \code{"xCH4"}. Must be normalised so xCH4 + xCO2 = 1.0 unless \code{cmethod = "total"}.
}

  \item{headspace}{
    (optional) a data frame or length-one numeric vector with bottle headspace volume(s).
    If a data frame is used, it should at least contain a \code{"id"} (bottle identification code) column (see \code{"id.name"}) and headspace volume column (see \code{vol.hs.name} argument).
    Required if \code{cmethod = "total"}.
    Default is \code{NULL}.
}

  \item{vol.hs.name}{
    name of column containing headspace volume data in optional \code{headspace} data frame. 
    Default is \code{"vol.hs"}.
}
 
 
 \item{cmethod}{
    method for calculating cumulative methane production.
    Use \code{"removed"} to base production on \code{xCH4} and gas volumes removed (default).
    Use \code{"total"} to base it on the sum of methane removed and methane remaining in the bottle headspace.
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
    If not, this argument adds a row with \code{time.name = 0} for each bottle in order to calculate production rates for the first observation.
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
 
  \item{dry}{
    set to \code{TRUE} if volume data are standardised to dry conditions (e.g., AMPTS II data). 
    The default (\code{FALSE}) means biogas is assumed to be saturated with water vapor.
  }

  \item{empty.name}{
    column containing a binary (logical, or integer or numeric (1 or 0)) variable indicating when accumulated biogas was emptied. 
    Use for mix of cumulative/interval data.
    If used, \code{interval} is ignored.
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
  Using volume data from \code{dat} and gas composition from \code{comp}, this function will calculate standardised biogas and methane production (if \code{comp} is provided) for each observation,  interpolating \code{comp.name} (from \code{comp} argument) to each \code{time.name} in \code{dat} if needed, and summing these for cumulative values. 
  All volumes (input and output) have the same units, e.g., mL, L, SCF.

  Biogas composition (\code{comp.name} column in \code{comp}) is specified as the mole fraction of methane in dry biogas, normalised so mole fractions of methane and carbon dioxide sum to unity (Richards et al. 1991). Alternatively, if \code{cmethod} is set to \code{"total"}, biogas composition is the mole fraction of methane in dry biogas (include all the other gases except water). 
 
  Standard values and units for temperature and pressure can be globally set using the function \code{\link{options}}.
  See \code{\link{stdVol}}.
  To surpress volume correction to a \dQuote{standard} temperature and pressure, leave \code{temp} as \code{NULL} (the default).

  If \code{check = TRUE}, the input values of mole fraction of methane in biogas are checked, and a warning is returned if the are outside 0, 1.

  See associated vignette (\code{calcBgVol_function.Rnw}) for more information.
  
}

\value{
  a data frame with all the columns originally present in \code{dat}, plus these others:
  \item{vBg}{Standardised volume of biogas production for individual event.}
  \item{xCH4}{Interpolated mole fraction of methane in biogas.}
  \item{vCH4}{Standardised volume of methane production for individual event. Only if \code{comp} is provided.}
  \item{vhsCH4}{Standardised volume of methane present in bottle headspace. Only if \code{method = "total"} is used.}
  \item{cvBg}{Standardised cumulative volume of biogas production.}
  \item{cvCH4}{Standardised cumulative volume of methane production. Only if \code{comp} is provided.}
  \item{rvBg}{Production rate of biogas.}
  \item{rvCH4}{Production rate of methane. Only if \code{comp} is provided.}
}
\references{
  Hafner, S.D., Rennuit, C., Triolo, J.M., Richards, B.K. 2015. Validation of a simple gravimetric method for measuring biogas production in laboratory experiments. \emph{Biomass and Bioenergy} \bold{83}, 297-301.

  Richards, B.K., Cummings, R.J., White, T.E., Jewell, W.J. 1991. Methods for kinetic analysis of methane fermentation in high solids biomass digesters. \emph{Biomass and Bioenergy} 1: 65-73.
}
\author{
  Sasha D. Hafner and Nanna Lojborg
}

\seealso{
  \code{\link{cumBg}},
  \code{\link{calcBgMan}},
  \code{\link{calcBgGD}},
  \code{\link{summBg}},
  \code{\link{interp}},
  \code{\link{stdVol}},
  \code{\link{options}}
}

\examples{ 
# Example with long structured input data frame

data("s3lcombo")
s3lcombo

# Calculate cumulative production and rates from s3lcombo
# With default data structure comp argument is not needed
# Necessary to extrapolate because first observations are missing xCH4
cbg <- calcBgVol(s3lcombo, 
                 temp = 25, pres = 1,
                 id.name = 'id', time.name = 'time.d', 
                 vol.name = 'vol.ml', comp.name = 'xCH4', 
                 extrap = TRUE)
head(cbg)

# Plot results
\dontrun{
  # Not run just because it is a bit slow
  ggplot(cbg, aes(time.d, cvCH4, colour = id)) + 
         geom_point() + geom_line(aes(group = id)) +
         labs(x = "Time (d)", y = "Cumulative methane production  (mL)", colour = "Bottle ID")  + 
         theme_bw() 

  plot(ggplot)
}


# Wide data structure, from AMPTS II in this case
data("feedVol")
head(feedVol)

# By default biogas is assumed to be saturated with water vapor
# Composition is set to a single value. 
# Data are cumulative
args(calcBgVol)
cbg <- calcBgVol(feedVol, comp = 1, temp = 0, pres = 1,
                 interval = FALSE, data.struct = 'wide',
                 id.name = "id", time.name = 'time.d', vol.name = '1', 
                 dry = TRUE)

head(cbg)


# Calculate cumulative production and rates from vol and comp
# Biogas volume and composition can be in separate data frames
data("vol")
data("comp")

head(vol)
head(comp)

# extrap = TRUE is needed to get CH4 results here because first xCH4 values are missing
cbg <- calcBgVol(vol, comp = comp, temp = 20, pres = 1, 
                 data.struct = "long",
                 id.name = "id", time.name = "days", comp.name = "xCH4", 
                 vol.name = "vol", extrap = TRUE)

head(cbg)


}
%% Add one or more standard keywords, see file "KEYWORDS" in the
%% R documentation directory.
\keyword{chron}
\keyword{manip}
\concept{biogas}
