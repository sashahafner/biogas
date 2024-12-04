\name{calcBgGrav}
\alias{calcBgGrav}
%- Also NEED an "\alias" for EACH other topic documented here.
\title{
  Calculate Biogas Production from Gravimetric Data
}
\description{
  \code{calcBgGrav} calculates cumulative biogas, methane production and production rates from mass loss (gravimetric measurements) and volume measurements for any number of bottles.
}
\usage{
calcBgGrav(
  # Main arguments
  dat,
  comp = NULL,              
  temp = NULL,
  pres = NULL,
  data.struct = 'longcombo',
  id.name = 'id',
  time.name = 'time',
  mass.name,    
  xCH4.name = 'xCH4', 
  xCO2.name = NULL,   
  xN2.name = NULL,    
  headspace = NULL,  
  vol.hs.name = 'vol.hs',
  temp.init = NULL, 
  pres.init = NULL,
  pres.resid = NULL, 
  headcomp = 'N2',  
  cmethod = 'removed',
  imethod = 'linear',
  extrap = FALSE,
  addt0 = TRUE,
  showt0 = TRUE,
  std.message = !quiet,
  check = TRUE,
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
      a data frame with at least bottle identification code, time of measurement (as \code{numeric}, or \code{POSIX}), and bottle mass.
      Structure depends on \code{data.struct} argument.
      Additional columns can be present--these will be returned in the output data frame.
  }
  
  \item{comp}{
      (optional) a data frame with the columns bottle identification code; time of measurement, (as \code{numeric}, or \code{POSIX}); and methane concentration within dry biogas as a mole fraction, considering only methane and carbon dioxide (unless \code{cmethod = "total"}) or a single numeric value.
      The names of these columns are specified with \code{id.name}, \code{time.name}, and \code{xCH4.name}.
      Default is \code{NULL}.
  }
  \item{temp}{
      the temperature of biogas just prior to venting.
      A length-one numeric vector.
      Degrees Celcius by default (see \code{unit.temp} argument). 
  }
  \item{pres}{
      the absolute pressure of biogas prior to venting.
      A length-one numeric vector or a character vector giving the name of the column in \code{dat} with the pressure measurements.
      Atmospheres by default (see \code{unit.pres} argument). 
  }
  
  \item{data.struct}{
      the structure of input data. The default of 'longcombo' means bottle mass and biogas composition (if available) are both in \code{dat}.
      The \code{dat} data frame must have bottle identification code and time columns with names specified with \code{id.name} and \code{time.name}, mass data in a single column with the name specified by \code{mass.name}, and biogas composition in a single column with the name specified by \code{comp.name}
      For the \code{data.struct = 'long'} option, two separate data frames are needed, one for mass and one for composition (if available).
      Each data frame must have bottle identification code and time columns with names specified with \code{id.name} and \code{time.name}.
      The \code{dat} data frame must have mass data in a single column with the name specified by \code{mass.name}.
      The \code{comp} data frame must have biogas composition in a single column with the name specified by \code{comp.name}.
      For the \code{data.struct = 'wide'} option, two separate data frames are needed as in \code{'long'}, but there are no bottle identification code columns.
      Instead, in \code{dat}, mass data are in a separate column for each bottle, and column names are bottle identification codes.
      Here, \code{mass.name} should be the name of the first column with mass data.
      All following columns are assumed to also have mass data.
      And in \code{comp}, biogas composition data are also in a separate column for each bottle, also with bottle identification codes for column names.
      Here, \code{comp.name} should be the name of the first column with biogas composition data, as for \code{dat}.
  }
  
  \item{id.name}{
      name of the bottle identification code column in \code{dat}. Must be the same in all data frames used in the function. Default is \code{"id"}.
  }
  
  \item{time.name}{
      name of column containing time data in \code{dat} and \code{comp} data frames. Default is \code{"time"}.
  }
  
  \item{mass.name}{
      name of column containing the primary response variable (bottle mass) in \code{dat} data frame.
  }
  
  \item{xCH4.name}{
      name of column containing biogas mole fraction of methane in \code{comp} data frame. Default is \code{"xCH4"}. Must be normalised so xCH4 + xCO2 = 1.0 unless \code{cmethod = "total"}.
  }
  
  \item{xCO2.name}{
      name of column containing biogas mole fraction of carbon dioxide in \code{comp} data frame. 
      Only needed if \code{cmethod = "total"}.
  }
  
  \item{xN2.name}{
      name of column containing biogas mole fraction of dinitrogen in \code{comp} data frame. 
      Only needed if \code{cmethod = "total"}.
  }
  
  \item{headspace}{
      (optional) a data frame or length-one numeric vector with bottle headspace volume(s).
      If a data frame is used, it should at least contain a \code{id} (bottle identification code) column (see \code{id.name}) and headspace volume column (see \code{vol.hs.name} argument).
      Default is \code{NULL}.
  }
  
  \item{vol.hs.name}{
      name of column containing headspace volume data in optional \code{headspace} data frame. 
      Default is \code{"vol.hs"}.
  }
  
  \item{temp.init}{
      the initial headspace temperature.
      Optional.
      A length-one numeric vector.
      Degrees Celcius by default (see \code{unit.temp} argument). 
      Default is \code{NULL}, which suppresses correction for initial temperature. 
  }  
  
  \item{pres.init}{
      the initial headspace pressure.
      Optional.
      A length-one numeric vector.
      Atmospheres by default (see \code{unit.pres} argument). 
      Default is \code{NULL}, which suppresses correction for initial pressure. 
  }  
  
  \item{pres.resid}{
      residual headspace pressure after venting.
      Optional.
      A length-one numeric vector.
      Atmospheres by default (see \code{unit.pres} argument). 
      Default is \code{NULL}. 
  }  
  
  \item{headcomp}{
      initial headspace composition as a chemical formula.
      Optional, for correcting for change in headspace density.
      Mixtures accepted.
      Default is \code{'N2'} for pure dinitrogen. 
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
   
  \item{std.message}{
      should a message with the standard conditions be displayed? 
      Default is \code{TRUE}. 
  }
    
  \item{check}{
    should input data be checked for unreasonable values (with warnings)? 
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
  
  \item{quiet}{
      use to suppress messages. Default is \code{FALSE}.
  }

}

\details{
  Using mass loss data from \code{dat} and composition data from \code{dat} or \code{comp}, this function will calculate standardized biogas and methane production for each observation using a grvimetric method.
  See reference below for details on the method.

  Standard values and units for temperature and pressure can be globally set using the function \code{\link{options}}.
  See \code{\link{stdVol}}.

}

\value{
  a data frame with all the columns originally present in \code{dat}, plus others including these:
  \item{vBg}{Standardized volume of biogas production for individual event.}
  \item{xCH4}{Calculated mole fraction of methane in biogas.}
  \item{vCH4}{Standardized volume of methane production for individual event.}
  \item{cvBg}{Standardized cumulative volume of biogas production.}
  \item{cvCH4}{Standardized cumulative volume of methane production.}
  \item{rvBg}{Production rate of biogas.}
  \item{rvCH4}{Production rate of methane.}
Units are based on units in input data.
}

\references{
  Hafner, S.D., Rennuit, C., Triolo, J.M., Richards, B.K. 2015. Validation of a simple gravimetric method for measuring biogas production in laboratory experiments. \emph{Biomass and Bioenergy} \bold{83}, 297-301.
}

\author{
  Sasha D. Hafner
}

\seealso{
  \code{\link{calcBgMan}},
  \code{\link{calcBgVol}},
  \code{\link{calcBgGD}},
  \code{\link{summBg}},
  \code{\link{interp}},
  \code{\link{stdVol}},
  \code{\link{options}}
}

\examples{ 

# Combined data frame with mass and composition for \code{data.struct = 'longcombo'} (default).

data('UQGravBiogas')
data('UQGravSetup')

cbg <- calcBgGrav(UQGravBiogas, temp = 35, pres = 1.5,
                   id.name = 'id', time.name = 'day', 
		   mass.name = 'mass.final',
                   xCH4.name = 'xCH4')

head(cbg)

# With separate mass and composition data frames
data('strawMass')
data('strawComp')
data('strawSetup')

cbg0 <- calcBgGrav(strawMass,
                   comp = strawComp, temp = 35, pres = 1.5,
                   data.struct = 'long', id.name = 'bottle',
                   time.name = 'time', mass.name = 'mass',
                   xCH4.name = 'xCH4')
warnings()

cbg <- calcBgGrav(strawMass,
                  comp = strawComp, temp = 35, pres = 1.5,
                  data.struct = 'long', id.name = 'bottle',
                  time.name = 'time', mass.name = 'mass',
                  xCH4.name = 'xCH4', extrap = TRUE)

head(cbg)

}
%% Add one or more standard keywords, see file "KEYWORDS" in the
%% R documentation directory.
\keyword{chron}
\keyword{manip}
\concept{biogas}
