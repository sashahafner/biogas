\name{calcBgGD}
\alias{calcBgGD}
%- Also NEED an "\alias" for EACH other topic documented here.
\title{
  Calculate Biogas Production from Gas Density Data
}
\description{
  \code{calcBgGD} (for \emph{calc}ulate \emph{b}io\emph{g}as from \emph{GD} (gas density) measurements) calculates cumulative biogas, methane production and production rates from mass loss and volume measurements for any number of bottles.
}
\usage{
cumBgVol(
  # Main arguments
  dat, 
  temp.vol,
  temp.grav,
  pres.vol,
  pres.grav,
  # Column names
  id.name, 
  time.name, 
  vol = 'vol',
  m.pres.name = NULL,
  m.post.name,
  comp.name = 'xCH4',       
  # Settings
  vented.mass = FALSE,
  averaging = 'interval',  
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
  addt0 = TRUE,
  extrap = FALSE,
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
    name of column containing time data in \code{dat} and \code{comp} data frames. 
}

  \item{vol.name}{
    name of the measured biogas volume column in \code{dat}. 
}
  \item{time.name}{
    name of column containing time data in \code{dat} and \code{comp} data frames. 
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

  See associated vignette (\code{cumBgVol_function.Rnw}) for more information.
  
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

  Hansen, T.L., Schmidt, J.E., Angelidaki, I., Marca, E., Jansen, J. la C., Mosbak, H. and Christensen, T.H. 2004. Method for determination of methane potentials of solid organic waste. \emph{Waste Management} \bold{24}, 393-400

  Richards, B.K., Cummings, R.J., White, T.E., Jewell, W.J. 1991. Methods for kinetic analysis of methane fermentation in high solids biomass digesters. \emph{Biomass and Bioenergy} 1: 65-73.
}
\author{
  Sasha D. Hafner and Nanna Løjborg
}

\seealso{
  \code{\link{cumBgMan}},
  \code{\link{cumBgGD}},
  \code{\link{summBg}},
  \code{\link{interp}},
  \code{\link{stdVol}},
  \code{\link{options}}
}

\examples{ 
# Example with long structured input data frame

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


# Example with wide structured input data frame
data("feedVol")

# By default biogas is assumed to be saturated with water vapor. For AMPTS II data, biogas volume are already standardized to dry conditions. 
# Therefore, dry argument is required as \texttt{dry = TRUE}.
# Composition is set to a single value. 
# Calculate cumulative production and rates from feedVol
cum.prod <- cumBgVol(feedVol, comp = 1, temp = 0, pres = 1,
                           data.struct = 'wide',
                           id.name = "id", time.name = 'time.d', 
                           dat.name = '1', comp.name = "xCH4",
                           dry = TRUE)
head(cum.prod)
# Note message about applying single value for biogas composition to all observations. 

# Data is cumulative. Set interval = FALSE, to indicate for cumulative data. 
cum.prod <- cumBgVol(feedVol, comp = 1, temp = 0, pres = 1,
                     interval = FALSE,
                     data.struct = 'wide',
                     id.name = "id", time.name = 'time.d', 
                     dat.name = '1', comp.name = "xCH4",
                     dry = TRUE)
head(cum.prod)

# Use default values for columns when possible 
cum.prod <- cumBgVol(feedVol, comp = 1, temp = 0, pres = 1,
                     interval = FALSE,
                     data.struct = 'wide',
                     time.name = 'time.d', 
                     dat.name = '1',
                     dry = TRUE)
head(cum.prod)

# Can leave out composition data, and then CH4 is not included in results
# NTS: does not work!
cum.prod <- cumBgVol(feedVol, temp = 0, pres = 1,
                     interval = FALSE,
                     data.struct = 'wide',
                     time.name = 'time.d', 
                     dat.name = '1',
                     dry = TRUE)
head(cum.prod)

# Leave out pres or temp, and results are not standardised
cum.prod <- cumBgVol(feedVol, comp = 1,
                     interval = FALSE,
                     data.struct = 'wide',
                     time.name = 'time.d', 
                     dat.name = '1',
                     dry = TRUE)
head(cum.prod)

# Plot results
\dontrun{
  # Not run just because it is a bit slow
  library(ggplot2)
  ggplot <- ggplot(cum.prod, aes(time.d, cvCH4, colour = id)) + 
    geom_point() +
    geom_line(aes(group = id)) +
    labs(x = "Time [d]", y = "Cumulative methane production  [mL]", colour = "Bottle id")  + 
    theme_bw() 
  
  plot(ggplot)
}


# Example with longcombo structured data
data("s3lcombo")

# Calculate cumulative production and rates from s3lcombo
cum.prod <- cumBgVol(s3lcombo, comp = comp, temp = 25, pres = 1, 
                        id.name = 'id', time.name = 'time.d', 
                        data.struct = 'longcombo', dat.name = 'vol.ml', 
                        comp.name = 'xCH4', extrap = TRUE)
head(cum.prod)

# Note composition argument is set as comp when data structure is longcombo to avoid errors regarding missing composition data frame 

# Plot results
\dontrun{
  # Not run just because it is a bit slow
  ggplot <- ggplot(cum.prod, aes(time.d, cvCH4, colour = id)) + 
    geom_point() +
    geom_line(aes(group = id)) +
    labs(x = "Time [d]", y = "Cumulative methane production  [mL]", colour = "Bottle ID")  + 
    theme_bw() 

  plot(ggplot)
}

}
%% Add one or more standard keywords, see file "KEYWORDS" in the
%% R documentation directory.
\keyword{chron}
\keyword{manip}
\concept{biogas}
