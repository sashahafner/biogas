\name{summBg}
\alias{summBg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Summarise and Normalise Cumulative Methane Production
}
\description{
    From cumulative gas production, use \code{summBg} to standardise, interpolate, subtract innoculum contribution, normalise by substrate mass, and summarise the output calculating mean and standard devations for each type of sample (groups of replicates). 
    The function is flexible: some, all, or none of these operations can be carried out in a call.
    Typically \code{summBg} is used to calculate biochemical methane potential (BMP) from cumulative methane production.
  }
\usage{
summBg(vol, setup, id.name = "id", time.name = "time", 
       descrip.name = "descrip", inoc.name = NULL, inoc.m.name = NULL,
       norm.name = NULL, norm.se.name = NULL,  vol.name = "cvCH4", 
       imethod = "linear", extrap = FALSE, when = 30, 
       when.min = 0, rate.crit = 'net', 
       show.obs = FALSE, show.rates = FALSE, show.more = FALSE,
       sort = TRUE, set.name = 'set', quiet = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vol}{
    a data frame with the columns bottle identification code; time of measurement (as \code{numeric}, or \code{POSIX}); and the response variable biogas volume or cumulative volume.
    The names of these columns can be specified with \code{id.name}, \code{time.name}, and \code{vol.name}. Or, the default names can be used. 
    Additional columns can be present-- these will be returned in the output data frame.
    Optional: as of version 1.11.5, \code{vol} can be a list of suitable data frames.
    This is convenient for comparing different approaches for calculating methane production, for example.
}
  \item{setup}{ 
    a data frame containing information to summarise, substract inoculum effect or normalise the data. Should contain at least the column \code{id.name} (bottle identification code), and the additional columns depending on the operations desired. See \sQuote{Details} for more information.
}
  
  \item{id.name}{
    name of the bottle identification code column in \code{vol} and \code{setup}. Should be the same in all data frames used in the function. Default is \code{"id"}.
}
  \item{time.name}{
    name of column containing time data in \code{vol}. Default is \code{"time"}.
}
  \item{descrip.name}{
    (optional) name of column containing a description of bottle substrate (or a code for this) in \code{setup} data frame. The summary of the data will be made following this column. Default is \code{"descrip"}.
}
  \item{inoc.name}{
    (optional) the value in the \code{descrip.name} (\code{setup} data frame) column that is used to describe the inoculum-only bottles. Length one character vector. Argument not used by default.  
}
  \item{norm.name}{
    (optional) the name of the column in \code{setup} that has the mass to perform the normalisation (typically substrate VS mass). Length one character vector. Argument not used by default.
}
  \item{norm.se.name}{
    (optional) the name of the column in \code{setup} that has the standard error of the data in the column \code{norm.name}. Length one character vector. If provided, this error in substrate mass will be included in the standard error and standard deviation given in the output. Argument not used by default.
}
  \item{inoc.m.name}{
    (optional) the name of the column in \code{setup} that has inoculum mass.
    If specified, these data are used to subtract the inoculum contribution to methane production.
    Length one character vector. Default is \code{"minoc"}. 
}
  \item{vol.name}{
    the name of the column(s) in \code{vol} that contains the response variable used for the summary--typically cumulative methane volume. 
    The default is \code{"cvCH4"} for \emph{c}umulative \emph{v}olume of methane (CH4).
    Optional: as of version 1.12.0, \code{vol.name} may be a vector with length > 1.
    In this case each response variable will be used and results will be returned for all, together in a single data frame.
    Character vector.
}
  \item{imethod}{
    the interpolation method to be used. 
    This is passed as the \code{method} argument to \code{\link{interp}}. Length one character vector. Default is \code{"linear"} for linear interpolation.
}
  \item{extrap}{
    should extrapolation be carried out? Set to \code{TRUE} if extrapolation wanted.
    Length one logical vector.
    This is passed as the \code{extrap} argument to \code{\link{interp}}. Default is \code{FALSE}
}
  \item{when}{
    value(s) of \code{time.name} in \code{vol} data frame at which the results should be evaluated.
    Alternatively, set to \code{"end"} (to return the latest values for each bottle) or \code{"meas"} (to return a value for each measured time for each bottle).
    To select a time for each bottle based on the methane production rate, use, e.g., \code{"1p3d"} for 1\% per day for at least 3 d (any values can be used, even something impractical like \code{"0.1p10d"}).
    In this case, the earliest time where production rate drops below 1\% of cumulative production per day for at least 3 days will be used.
    Where this time differs for multiple bottles with the same \code{descrip.name}, the latest one will be used.
    Numeric or character vector or list (use a list for, e.g., \code{list("1p3d", "end", 30)}).
}
  \item{when.min}{
    minimum duration (value of \code{when}) to be used with a relative duration criterion, e.g., if you want \code{"1p3d"} but no less than 15 days set \code{when.min = 15}. Ignored if \code{when} is numeric. Default is \code{Inf} (no limit).
}
  \item{rate.crit}{
    type of rate criterion to be applied for relative \code{when}, e.g., \code{when = "1p3d"} and \code{rate.crit = "net"} is based on net gas production (after subtracting incoulum contribution). Options are \code{"net"} or \code{"gross"}.
}
  \item{show.obs}{
    set to \code{TRUE} to return all observations, otherwise means and standard deviations are returned. Default is \code{FALSE}. 
}
  \item{show.rates}{
    set to \code{TRUE} to return all observations with relative rates. Used to check rates for evaluation time. The \code{when} argument will be ignored if \code{show.rates = TRUE}. Default is \code{FALSE}. 
}
  \item{show.more}{
    set to \code{TRUE} to return additional details, including fraction of total methane produced by inoculum. Default is \code{FALSE}. 
}
  \item{sort}{
    controls sorting of results, which is by \code{descrip.name} and then \code{time.name} by default. Set to \code{FALSE} to use order of \code{descrip.name} from the \code{setup} data frame. Default is \code{TRUE}.
}
  \item{set.name}{
    column name in result for set of observations, corresponding to elements in \code{vol} when it is a list.
}
  \item{quiet}{
    use to suppress messages. Default is \code{FALSE}.
}
}
\details{
  \code{summBg} was primarily designed to calculate the biochemical methane potential (BMP) from cumulative methane production of a set of batch bottles through these steps: interpolation of cumulative production to a specified time (if needed), subtratction of apparent innoculum contribution, normalisation of the results by substrate mass (typically volatile solids (VS) mass, but could be the mass of anything within the bottle) and calculation of mean and standard deviation for each sample type (set of replicates, identified by \code{descrip.name}). 

If needed \code{summBg} can return values for all observations and be used for simpler operations e.g., determining cumulative biogas production at some specified time or normalising gas volume by different substrate characteristics.

To summarise data, the \code{setup} data frame should have a column with a description of bottle substrate (or a code for this). 
The name of the column is set by the \code{descrip.name} argument.  
If the inoculum effect is to be subtracted out, a column named \code{descrip.name} (for identifying replicates) and a column with the mass of inoculum present (any units), with a name set by \code{inoc.m.name} are both required in \code{setup}. 
To normalise by substrate mass (or any mass to be used for normalisation), an additional column with the mass of substrate is needed in \code{setup} --its name is set by \code{norm.name}.
  
This function is probably easier to understand by example.
See \sQuote{Examples}.
}
\value{
  a data frame, with the colums:
  \item{descrip}{from the input data frame setup}
  \item{mean}{mean of the response variable \code{vol.name}, typically after subtraction of inoculum contribution and normalization by substrate mass}
  \item{sd}{standard deviation of the response variable \code{vol.name}}
  \item{se}{standard error of the response variable \code{vol.name}}
  \item{n}{number of bottles}
  If \code{show.more = TRUE} additional columns are returned:
      summ2 <- summ2[ , c(descrip.name, time.name, 'mean', 'se', 'sd', 'n', 'rsd.inoc', 'fv.inoc', 'se1', 'se2', 'se3')]
  \item{rsd.inoc}{relative standard deviation in specific \code{vol.name} (methane) production by inouclum among replicates} 
  \item{fv.inoc}{(mean) fraction of bottle \code{vol.name} (methane) estimated to have come from inoculum} 
  \item{se1}{standard error contribution from variation among replicate substrate bottles} 
  \item{se2}{standard error contribution from variation among inoculum-only bottles} 
  \item{se2}{standard error contribution from uncertainty in substrate mass addition} 
  If \code{show.obs = TRUE} even more columns are returned:
  \item{cvCH4.tot}{name based on \code{vol.name}, total \code{vol.name} produced by bottle, normalization applied as for \code{vol.name} (and may be nonsensical)} 
  \item{cvCH4.inoc}{name based on \code{vol.name}, \code{vol.name} produced by bottle estimated as coming from inoculum, normalization applied as for \code{vol.name} (and may be nonsensical)} 
  \item{se.inoc}{standard error from inoculum used to calculate \code{se2}} 
  \item{cvCH4.se}{name based on \code{vol.name}, standard error from substrate mass used to calculate \code{se3}} 
}
\author{
  Sasha D. Hafner and Charlotte Rennuit
}
\note{
  Reported standard deviation and standard error includes an estimate of variability from subtracting the inoculum contribution when this is done.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{cumBg}}
}
\examples{
data("vol")
data("comp") 
data("setup") 

# First need to calculate cumulative methane production data
cum.prod <- cumBg(vol, comp = comp, temp = 20, pres = 1, 
                  time.name = "days", extrap = TRUE)

head(cum.prod)

# Cumulative methane production (default) at 30 d 
# Uses default names for some columns
summBg(vol = cum.prod, setup = setup, time.name = "days", when = 30)

# Or total cumulative biogas
summBg(vol = cum.prod, setup = setup, time.name = "days", 
       vol.name = "cvBg", when = 30)

# Cumulative CH4 only, subtract inoculum contribution
summBg(vol = cum.prod, setup = setup, time.name = "days", 
       inoc.name = "inoc", inoc.m.name = "minoc", when = 30)

# And normalise by mvs.sub column (mass of substrate VS here) (so the result is BMP)
summBg(vol = cum.prod, setup = setup, time.name = "days", 
       inoc.name = "inoc", inoc.m.name = "minoc", when = 30, 
       norm.name = "mvs.sub")

# Same example, but return results for three times
summBg(vol = cum.prod, setup = setup, time.name = "days", 
       inoc.name = "inoc", inoc.m.name = "minoc", when = c(10, 30, 60), 
       norm.name = "mvs.sub")

# Back to earlier example, but return all individual observations 
# (and total production and individual contributions of substrate 
# and inoculum) 
summBg(vol = cum.prod, setup = setup, time.name = "days", 
       inoc.name = "inoc", inoc.m.name = "minoc", when = 30, 
       norm.name = "mvs.sub", show.obs = TRUE)

# Something different: interpolated biogas production rates
summBg(vol = cum.prod, setup = setup, time.name = "days", 
       vol.name = "rvBg", when = 30, show.obs = TRUE)

# The when argument could also be 'meas', 'end', or '1p3d' (or related) for any of these examples 
summBg(vol = cum.prod, setup = setup, time.name = "days", 
       inoc.name = "inoc", inoc.m.name = "minoc", when = "end", 
       norm.name = "mvs.sub")

summBg(vol = cum.prod, setup = setup, time.name = "days", 
       inoc.name = "inoc", inoc.m.name = "minoc", when = "meas", 
       norm.name = "mvs.sub")

summBg(vol = cum.prod, setup = setup, time.name = "days", 
       inoc.name = "inoc", inoc.m.name = "minoc", when = "1p3d", 
       norm.name = "mvs.sub")

# Or combine multiple values of when in a list

summBg(vol = cum.prod, setup = setup, time.name = "days", 
       inoc.name = "inoc", inoc.m.name = "minoc", 
       when = list(30, "1p3d", "end"), 
       norm.name = "mvs.sub")

# If you want to apply the 1% criterion but also want a single fixed time for all bottles,
# you have to use two calls
summBg(vol = cum.prod, setup = setup, time.name = "days", 
       inoc.name = "inoc", inoc.m.name = "minoc", when = "1p3d", 
       norm.name = "mvs.sub")

# From the first call, the longest time is 42 days, so use when = 42
summBg(vol = cum.prod, setup = setup, time.name = "days", 
       inoc.name = "inoc", inoc.m.name = "minoc", when = 42, 
       norm.name = "mvs.sub")

# If an error is thrown because a bottle doesn't meet the 1% criterion, use 
# show.rates = TRUE to see rates
\dontrun{
# Will return error
cpshort <- cum.prod[cum.prod$days < 10, ]
summBg(vol = cpshort, setup = setup, time.name = "days", 
       inoc.name = "inoc", inoc.m.name = "minoc", when = "1p", 
       norm.name = "mvs.sub")
}

# So then use this to see which bottles are causing problems
cpshort <- cum.prod[cum.prod$days < 10, ]
summBg(vol = cpshort, setup = setup, time.name = "days", 
       inoc.name = "inoc", inoc.m.name = "minoc", when = "1p", 
       norm.name = "mvs.sub", show.rates = TRUE)


# Example with dataset with different column names
data("vol2")
data("comp2")
data("setup2")

# First need to calculate cumulative methane production data
cum.prod <- cumBg(vol2, comp = comp2, temp = 20, pres = 1, 
                  id.name = "bottle", time.name = "days", 
		  dat.name = "meas.vol", comp.name = "CH4.conc", 
                  extrap = TRUE)

head(cum.prod)

# Cumulative CH4 production at 30 d, subtract inoculum contribution 
# and normalise by sub.vs column (mass of substrate VS here) (look at setup2).
summBg(vol = cum.prod, setup = setup2, id.name = "bottle", 
       time.name = "days", descrip.name = "description", 
       inoc.name = "Inoculum", inoc.m.name = "inoc.mass", 
       norm.name = "sub.vs", when = 30)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
\keyword{manip}
\keyword{chron}
\concept{biogas}
