\name{interp}
\alias{interp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Interpolate Biogas Composition or Cumulative Production
}
\description{
  \code{interp} interpolates (or extrapolates) biogas composition (methane concentration) or cumulative production data to a wanted time using one of several possible methods. 
}
\usage{
interp(times, y, time.out, method = "linear", extrap = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{times}{
    measurement times.
    Numeric vector or \code{POSIX}.
}
  \item{y}{
    response variable at \code{times}, e.g., mole fraction of methane in biogas, or biogas cumulative production.
    Numeric vector.
}
  \item{time.out}{
    time or times at which interpolated values are needed.
    Numeric vector or \code{POSIX}.
}
  \item{method}{
    method used for interpolation.
    Default is \code{"linear"}, which uses the \code{\link{approx}} function.
    For spline interpolation, use \code{"ffm"} or the other options available (see \code{\link{spline}}).
}
  \item{extrap}{
    should \code{y} be exptrapolated?
    Logical. Default is \code{FALSE}. 
    See \sQuote{Details}.
}
}
\details{
  \code{interp} is really a wrapper for the interpolation functions \code{\link{approx}} and \code{\link{spline}}.
  For cumulative production, which (usually) must monotonically increase, \code{method = "hyman"} is the best choice.
  Extrapolation behavior depends on \code{method}.
  For \code{method = "linear"}, \code{extrap = TRUE} simply returns the value of the closest \code{y}.
  See \code{\link{approx}} and \code{\link{spline}} for more information.
}
\value{
    Interpolated estimates of \code{y} at given \code{times}.
}
\author{
  Sasha D. Hafner and Charlotte Rennuit
}

\seealso{
  \code{\link{cumBg}}
}
\examples{
# Fake composition data 
dat <- data.frame(time = c(1, 7, 14, 28), 
                  xCH4 = c(0.3, 0.5, 0.61, 0.65))
interp(dat$time, dat$xCH4, time.out = 10)
interp(dat$time, dat$xCH4, time.out = 10, method = "natural")
interp(dat$time, dat$xCH4, time.out = c(10, 30))
interp(dat$time, dat$xCH4, time.out = c(10, 30), method = "natural")
interp(dat$time, dat$xCH4, time.out = c(10, 30), extrap = TRUE)

# Actual data
data(comp)
# Work with one reactor
bgc <- subset(comp, id=="2_1")

# With numeric time, interpolate to 1, 7, and 30 days
interp(bgc$days, bgc$xCH4, time.out = c(1, 7, 30))

# If extrapolation is OK
interp(bgc$days, bgc$xCH4, time.out = c(1, 7, 30), extrap = TRUE)

# Or POSIXct
interp(bgc$date.time, bgc$xCH4, 
       time.out = as.POSIXct("2014-07-12 13:00:00"))

# For cumulative gas production
data(vol)
# Work with one reactor
bgv <- subset(vol, id=="2_1")
# Calculate cumulative volume
bgv <- cumBg(bgv, time.name = "days")
# The interpolate of cumulative production to 1, 7, and 30 days
interp(bgv$days, bgv$cvBg, time.out = c(1, 7, 30), method = "hyman")
}
% Add one or more standard keywords, see file "KEYWORDS" in the
% R documentation directory.
\keyword{chron}
\keyword{smooth}
\concept{biogas}
