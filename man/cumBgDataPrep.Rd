\name{cumBgDataPrep}
\alias{cumBgDataPrep}
%- Also NEED an "\alias" for EACH other topic documented here.
\title{
  Preparation of Biogas Data 
}
\description{
  \code{cumBgDataPrep} sorts and restructures biogas data for further calculation of cumulative biogas, methane production and production rates. 
}

\usage{
cumBgDataPrep(
  # Main arguments
  dat, dat.type = 'vol', comp = NULL, 
  temp = NULL, pres = NULL, 
  interval = TRUE,
  data.struct = 'longcombo',
  # Column names
  id.name = 'id', time.name = 'time', dat.name = dat.type,
  comp.name = 'xCH4',
  headspace = NULL, vol.hs.name = 'vol.hs',
  # Calculation method and other settings
  imethod = 'linear', extrap = FALSE,
  # Additional argument for volumetric data only 
  empty.name = NULL,
  # Warnings and messages
  std.message = !quiet,
  check = TRUE, quiet = FALSE
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
    For mixed manometric measurements (some observations vented, some not), use \code{interval = TRUE}, include a column for \code{pres.resid}, and, if composition was only measured for vented observations, use \code{imethod = "f1"}.
    Default is \code{TRUE}.
  }

  \item{data.struct}{
    the structure of input data. The default of 'longcombo' means separate objects for volume and composition (if available). The composition column is in the \code{dat} data frame, and the \code{comp} argument should be set as comp. 
    The \code{dat} data frame must have reactor identification code and time columns with names specified with \code{id.name} and \code{time.name}, volume data in a single column with the name specified by \code{dat.name}, and biogas composition in a single column with the name specified by \code{comp.name}
    For the \code{data.struct = "long"} option, two separate data frames are needed with separate objects for volume and composition (if available).
    Each data frame must have reactor identification code and time columns with names specified with \code{id.name} and \code{time.name}.
    The \code{dat} data frame must have volume data in a single column with the name specified by \code{dat.name}.
    The \code{comp} data frame must have biogas composition in a single column with the name specified by \code{comp.name}.
    For the \code{data.struct = "wide"} option, two separate data frames are needed as in \code{"long"}, but there are no reactor identification code columns.
    Instead, in \code{dat}, volume data are in a separate column for each bottle, and column names are reactor identification codes.
    Here, \code{dat.name} should be the name of the first column with volume data.
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
  \item{dat.name}{
    name of column containing the primary response variable (volume or mass) in \code{dat} data frame. Default is \code{dat.type} value. See \code{dat.type} argument.
}
  \item{comp.name}{
    name of column containing biogas mole fraction of methane in \code{comp} data frame. Default is \code{"xCH4"}. Must be normalised so xCH4 + xCO2 = 1.0 unless \code{cmethod = "total"}.
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

  \item{empty.name}{
    column containing a binary (logical, or integer or numeric (1 or 0)) variable indicating when accumulated biogas was emptied. 
    Use for mix of cumulative/interval data.
    Only applies to volumetric data (dat.type = 'vol').
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
  }
 
  \item{quiet}{
    use to suppress messages. Default is \code{FALSE}.
}

}
\details{
  This function prepares biogas data by sorting and restructuring (if required) volume, mass, pressure, or molar quantity data from \code{dat} and gas composition from \code{comp} internally in the cumBg* functions, to match requirements for calculating cumulative biogas and methane production and production rates.
 
 Data preparation is done in a xx-step process
 
 First "wide" and "longcombo" data are restructured to "long" structured data frames. If data are cumulative, rows with missing respons variables are removed.
Data structure and similar proporsitions are defined in and passed from cumBg* functions. Though, the \code{dat.type} argument needs to be specified directly in the cumBgDataPrep() call. 
"wide" date are restructured to "long" format by initially defining number of bottles and then defining their names from the column names. Then missing dat values are omitted and a fixed id name is created. Similar, approach is used to restructure the related \code{comp} data frame to "long" data structure.   
"longcombo" data are restructured to "long" by dividing the data frame in two separate: \code{dat} and \code{comp}, where \code{comp} consists \code{id.name, time.name, comp.name} and \code{dat} consists of all original columns except from \code{comp.name}.
 
 Secondly, composition data in the now "long" structured data frame are sorted unless measuring method is GCA (GCA method has no biogas composition data) and finally restructured to "longcombo" format, which is required for cumBg* to evaluate the data.  
 % Describe how "long" data is restructured to "longcombo" in this section
 
 First step in final sorting is to ignore first observation for mass data followed by dropping NAs from \code{comp}. 
 
 This is followed by interpolation of gas composition to match the times of volume measurements. If only one xCH4 value is provided in the data frame, use it for all observations if \code{extrap = TRUE} or times match. If times does not match, a warning will be returned. Similar, if the \code{comp} argument is set to a single methane value, this value is used for all observations. If no composition data is provided, NA is used.     

 Subsequently, headspace is added if provided and composition data are corrected if they seems to be percentage. Again GCA method is excluded from composition correction, as no composition data are provided from this method.
 
 Finally, potential mixed interval/cumulative data in the now "longcombo" structured is sorted followed by standardizing vBg and summarizing final volumes to get cumulative volumes, then interval from them. 


}


\references{
  Hafner, S.D., Rennuit, C., Triolo, J.M., Richards, B.K. 2015. Validation of a simple gravimetric method for measuring biogas production in laboratory experiments. \emph{Biomass and Bioenergy} \bold{83}, 297-301.

  Hansen, T.L., Schmidt, J.E., Angelidaki, I., Marca, E., Jansen, J. la C., Mosbak, H. and Christensen, T.H. 2004. Method for determination of methane potentials of solid organic waste. \emph{Waste Management} \bold{24}, 393-400

  Richards, B.K., Cummings, R.J., White, T.E., Jewell, W.J. 1991. Methods for kinetic analysis of methane fermentation in high solids biomass digesters. \emph{Biomass and Bioenergy} 1: 65-73.
}
\author{
  Sasha D. Hafner and Nanna Løjborg
}

\seealso{
  \code{\link{summBg}},
  \code{\link{cumBgVol}},
  \code{\link{cumBgMan}},
  \code{\link{cumBgGD}},
  \code{\link{interp}},
  \code{\link{stdVol}},
  \code{\link{options}}
}

\examples{
# Example longcombo volume data (volumetric method)
data("s3lcombo")

head(s3lcombo)

longcombo <- cumBgDataPrep(s3lcombo, temp = 25, pres = 1,
                      data.struct = "longcombo",
                      id.name = "id", time.name = "time.d",
                      dat.name = "vol.ml", comp.name = "xCH4",
                      extrap = TRUE)

# Use default values for data.struct, id.name, and comp.name
longcombo <- cumBgDataPrep(s3lcombo, temp = 25, pres = 1,
                      time.name = "time.d", dat.name = "vol.ml",
                      extrap = TRUE)
head(longcombo)


# Example long volume data
data("s3lcombo")
data("s3compl")

head(s3lcombo)
head(s3compl)

long <- cumBgDataPrep(s3lcombo, comp = s3compl, temp = 25, pres = 1,
                      time.name = 'time.d', dat.name = "vol.ml",
                      data.struct = 'long',
                      extrap = TRUE)
head(long)


# Example wide volume data
data("s3volw")
data("s3compw")

head(s3volw)
head(s3compw)

wide <- cumBgDataPrep(s3volw, comp = s3compw, temp = 25, pres = 1,
                      time.name = 'time.d',
                      data.struct = 'wide',
                      dat.name = 'D', comp.name = 'D',
                      extrap = TRUE)
head(wide)


# Example long mass data (gravimetric method)
data("strawMass")
data("strawComp")

head(strawMass)
head(strawComp)

# Need to specify data type with \code{dat.type} argument.
grav.long <- cumBgDataPrep(strawMass, comp = strawComp, dat.type = "mass",
                           temp = 31, pres = 1, data.struct = "long",
                           id.name = "bottle",
                           headspace = strawSetup, vol.hs.name = "headspace",
                           extrap = TRUE)


# Example longcombo pressure data (manometric method)
data("sludgeTwoBiogas")
data("sludgeTwoSetup")

head(sludgeTwoBiogas)
head(sludgeTwoSetup)

pres.longcombo <- cumBgDataPrep(sludgeTwoBiogas, dat.type = "pres",
                                temp = 30,
                                time.name = "time.d", 
                                headspace = sludgeTwoSetup) 

}
%% Add one or more standard keywords, see file "KEYWORDS" in the
%% R documentation directory.
\keyword{chron}
\keyword{manip}
\concept{biogas}
