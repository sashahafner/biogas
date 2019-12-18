\name{calcBgGD}
\alias{calcBgGD}
%- Also NEED an "\alias" for EACH other topic documented here.
\title{
  Calculate Biogas Production from Gas Density Data (GD-BMP)
}
\description{
  \code{calcBgGD} (for \emph{calc}ulate \emph{b}io\emph{g}as from \emph{GD} (gas density) measurements) calculates cumulative biogas, methane production and production rates from mass loss and volume measurements for any number of bottles.
}
\usage{
calcBgGD(
  # Main arguments
  dat, 
  temp.vol,
  temp.grav,
  pres.vol,
  pres.grav,
  # Column names
  id.name, 
  time.name, 
  vol.name,
  m.pre.name = NULL,
  m.post.name,
  comp.name = 'xCH4',       
  # Settings
  vented.mass = FALSE,
  averaging = 'final',  
  temp.init = NULL,       
  pres.init = NULL,      
  headspace = NULL,     
  vol.hs.name = NULL,  
  headcomp = 'N2',    
  # Calculation method and other settings
  vmethod = 'vol',   
  comp.lim = c(0, 1), 
  comp.sub = NA,     
  imethod = 'linear',
  extrap = FALSE,
  addt0 = TRUE,
  showt0 = TRUE,
  dry = FALSE,
  # Warnings and messages
  std.message = TRUE,
  check = TRUE,
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
    a data frame with bottle identification code, time of measurement (as \code{numeric}, or \code{POSIX}), bottle mass, and measured biogas volume.
    See Details section for details on units.
    Additional columns can be present--these will be returned in the output data frame.
}

  \item{temp.vol}{
    temperature at which biogas volume was measured.
}

  \item{temp.grav}{
    temperature of bottle headspace at time of biogas venting, prior to gravimetric measurement.
}

  \item{pres.vol}{
    pressure at which biogas volume was measured.
}

  \item{pres.grav}{
    pressure of bottle headspace at time of biogas venting, prior to gravimetric measurement.
}

  \item{id.name}{
    name of the bottle identification code column in \code{dat}. Must be the same in all data frames used in the function. 
}
  \item{time.name}{
    name of column containing time data (cumulative time) in \code{dat}. 
}

  \item{vol.name}{
    name of the measured biogas volume column in \code{dat}. 
}
  \item{m.pre.name}{
    name of column containing pre-venting bottle mass in \code{dat} data frame. 
    Optional, required for \code{vented.mass = TRUE} and to calculate leakage.
}

  \item{m.post.name}{
    name of column containing post-venting bottle mass in \code{dat} data frame. 
    See details.
}

  \item{comp.name}{
    name of column with biogas composition to be added to output data frame. 
}

  \item{vented.mass}{
    Set to \code{TRUE} to use vented mass loss (GD_v method) instead of total mass loss (GD_t method) in calculations.
    See details.
}

  \item{averaging}{
   type of averaging used for calculating biogas composition.
   Default is \code{final}.
    See details.
}

  \item{temp.init}{
    optional initial headspace temperature.
    Used to correct results for initial headspace.
    See details.
}

  \item{pres.init}{
    optional initial headspace pressure.
    Used to correct results for initial headspace.
    See details.
  }

  \item{headspace}{
    optional data frame or length-one numeric vector with reactor headspace volume(s).
    If a data frame is used, it should at least contain a \code{"id"} (reactor identification code) column (see \code{"id.name"}) and headspace volume column (see \code{vol.hs.name} argument).
    Required if \code{cmethod = "total"}.
    Default is \code{NULL}.
  }

  \item{vol.hs.name}{
    optional name of column containing headspace volume data in optional \code{headspace} data frame. 
  }

  \item{headcomp}{
    optional initial headspace composition used to correct results for initial headspace.
    Default of \code{"N2"} is only logical value.
  }

  \item{vmethod}{
    method used for calculating biogas volume.
    Default of \code{'vol'} is based on measured biogas volume in \code{vol.name} column.
    Alternative is \code{'grav'} for gravimetric method, which should be used with \code{vented.mass = TRUE}.
  }

  \item{comp.lim}{
    acceptable limits on calculated methane mole fraction.
    Any values outside of this range are set to \code{comp.sub}.
    Default of \code{c(0, 1)}.
    Length two vector.
  }

  \item{comp.sub}{
    Value substituted in for calculated methane mole fraction when calculated value is outside of \code{comp.lim} range.
    Length one vector.
    Default value of \code{NA} should generally never be changed.
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
 
  \item{dry}{
    set to \code{TRUE} if volume data are standardized to dry conditions.
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
    Default is \code{"C"} for degrees Celcius. 
    Argument is passed to \code{\link{stdVol}}.
  }

    \item{unit.pres}{
    pressure units for \code{pres} and \code{pres.std} arguments.
    Default is \code{"atm"}.
    Argument is passed to \code{\link{stdVol}}.
  }

}

\details{
  Using volume and mass loss data from \code{dat}, this function will calculate standardized biogas and methane production for each observation using the gas density (GD) method.
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
  Justesen, C.G., Astals, S., Mortensen, J.R., Thorsen, R., Koch, K., Weinrich, S., Triolo, J.M., Hafner, S.D. 2019. Development and validation of a low-cost gas density method for measuring biochemical potential (BMP) \emph{Water (MDPI)} \bold{11(12)}: 2431.
}

\author{
  Sasha D. Hafner, Camilla Justesen, Jacob Mortensen
}

\seealso{
  \code{\link{calcBgMan}},
  \code{\link{calcBgVol}},
  \code{\link{summBg}},
  \code{\link{interp}},
  \code{\link{stdVol}},
  \code{\link{options}}
}

\examples{ 

data("UQGDBiogas")
data("UQGDSetup")

head(UQGDBiogas)
head(UQGDSetup)

cbg <- calcBgGD(UQGDBiogas,
                temp.vol = 20, pres.vol = 1013.25,
                temp.grav = 30, pres.grav = 1500,
                id.name = 'id', vol.name = 'vol',
                m.pre.name = 'mass.init', m.post.name = 'mass.final',
                time.name = 'time.d', unit.pres = 'mbar')


BMP <- summBg(cbg, UQGDSetup, id.name = "id",
              time.name = 'time.d', descrip.name = 'descrip',
              inoc.name = "Inoculum", inoc.m.name = "m.inoc", norm.name = "m.sub.vs",
              when = 'end')

BMP

}
%% Add one or more standard keywords, see file "KEYWORDS" in the
%% R documentation directory.
\keyword{chron}
\keyword{manip}
\concept{biogas}
