\name{fitFOM}
\alias{fitFOM}
%- Also NEED an "\alias" for EACH other topic documented here.
\title{
  Fit a First-Order Model to Biogas or Other Data
}
\description{
  \code{fitFOM} (FOM is for \emph{f}irst \emph{o}rder \emph{m}odel) is a flexible function for fitting first-order models to batch biogas production data, typically from a biochemical methane potential (BMP) test.
}
\usage{
fitFOM(dat, n.pool = 1, time.name = 'time.d', resp.name = 'cvCH4',
       fit.to = 'yield', method = 'Nelder-Mead', abs.err = FALSE, trans = TRUE,
       init = if (n.pool == 1) c(B = 'yield', k = 0.5) 
                else c(B = 'yield', f = 0.5, k1 = 0.01, k2 = 0.5), 
       fixed = NULL, fit.last = FALSE, lower = NULL, upper = NULL, lag.phase = FALSE)
}

%- maybe also "usage" for other objects documented here.
\arguments{
  \item{dat}{
    a data frame with columns for elapsed time and cumulative methane yield ("specific methane production" SMP) or another response variable.
  }
  \item{n.pool}{
    Number of substrate pools to include in the model.
    1 or 2.
    Length-one numeric vector.
  }
  \item{time.name}{
    Name of the column in \code{dat} that has elapsed time.
    Length-one character vector.
  }
  \item{resp.name}{
    Name of the column in \code{dat} that has response variable, typically cumulative methane yield ("specific methane production" SMP).
    Length-one character vector.
  }
  \item{fit.to}{
    Should fitting be to cumulative yield (\code{'yield'}, default) or the average production rate (\code{'rate'}, calculated as its difference over time)?
    Length-one character vector.
  }
  \item{method}{
    Method used to fit the model, i.e., to estimate best-fit parameter values.
    Default of \code{'Nelder-Mead'} uses that method via the \code{\link{optim}} function.
    The other \code{method}s available for \code{link{optim}} are alternatives.
    A better option seems to be \code{'LM'}, which uses the \code{nls.lm} function from the minpack.lm package; this is not the default option only because it requires another package.
    Length-one character vector.
  }
  \item{abs.err}{
    Should fitting be based on mean absolute error (\code{TRUE}) or sum of squares (\code{FALSE}, default).
    Length-one logical vector.
  }
  \item{trans}{
    Should parameter values be transformed for fitting?
    If \code{TRUE}, the \code{k} parameter(s) are log10-transformed and (if relevant) \code{f} is transformed with the standard logistic function.
    This ensures logical values (positive \code{k}, \code{f} between 0 and 1) and may result in better estimates.
    Default is \code{FALSE}.
    Length-one logical vector.
  }
  \item{init}{
    Vector of initial parameter estimates.
    See defaults for elements.
    For \code{B} parameter, set to \code{'yield'} to take the initial estimate from the \code{resp.name} column.
    Length-two or -four named numeric vector.
  }
  \item{fixed}{
    Fixed parameters that should be excluded from optimization.
    Named numeric vector.
  }
  \item{fit.last}{
    Set to \code{TRUE} to force fit exactly through final measured observation.
    Not recommended.
    Length-one logical vector.
  }
  \item{lower}{
    Optional lower parameter value limits.
    Only available with some \code{method}s.
    Named numeric vector.
  }
  \item{upper}{
    Optional upper parameter value limits.
    Only available with some \code{method}s.
    Named numeric vector.
  }
  \item{lag.phase}{
    Should the "lag phase" be excluded from fitting?
    This period is defined as all observations prior to maximum average production rate.
    Default of \code{FALSE}.
    Length-one logical vector.
  }
}
\details{
  Use for fitting a first-order model (estimation of best-fit parameter values). 
  Intended for extracting kinetic constants and maximum methane potential from biochemical methane potential (BMP) test measurements.
}

\value{
  A list with parameter estimates and additional information.
  Most useful elements are \code{coefs} and \code{coef.tab} (best-fit parameter values), \code{summ} (a summary that includes model efficiency, error, and convergence information), and \code{pred} (model predicted or fitted values).
}

\references{
  Hafner, S.D., Koch, K., Carrere, H., Astals, S., Weinrich, S., Rennuit, C. 2018. Software for biogas research: Tools for measurement and prediction of methane production. \emph{SoftwareX} \bold{7} 205-210. \doi{10.1016/j.softx.2018.06.005}
}

\author{
  Sasha D. Hafner
}

%\seealso{
%}

\examples{ 
# First use example data to generate a specific methane potential (SMP) curve
library(biogas)
data('feedVol')
data('feedSetup')

# Cumulative biogas and CH4 production
cbg <- calcBgVol(feedVol, comp = 1, temp = 0, pres = 1,
                 interval = FALSE, data.struct = 'wide',
                 id.name = 'id', time.name = 'time.d', vol.name = '1', 
                 dry = TRUE)

# Get SMP
SMP <- summBg(vol = cbg, setup = feedSetup, time.name = 'time.d', 
               inoc.name = 'BK', inoc.m.name = 'm.inoc', when = 'meas',
               norm.name = 'm.sub.vs', show.obs = TRUE)

# Select bottle 9
s9 <- subset(SMP, id == 9)

# Fit model
mod1 <- fitFOM(s9, n.pool = 1, time.name = 'time.d', resp.name = 'cvCH4')

# View summary
mod1$summ

# Add model predictions
s9$cvCH4.pred <- mod1$pred

# And plot
plot(cvCH4 ~ time.d, data = s9, type = 'o')
lines(cvCH4.pred ~ time.d, data = s9, col = 'red')

# Fit to rates instead
mod1b <- fitFOM(s9, n.pool = 1, time.name = 'time.d', resp.name = 'cvCH4', fit.to = 'rate')
mod1b$summ

# Try 2 pools
mod2 <- fitFOM(s9, n.pool = 2, time.name = 'time.d', resp.name = 'cvCH4')
mod2$summ

# First pool effectively ignored in the fit
# Try different method (this one required minpack.lm package)
\dontrun{
mod2 <- fitFOM(s9, n.pool = 2, time.name = 'time.d', resp.name = 'cvCH4', method = 'LM')
mod2$summ
}

# Unfortunately, here is a big effect of method on the result!

s9$cvCH4.pred2 <- mod2$pred

plot(cvCH4 ~ time.d, data = s9, type = 'o')
lines(cvCH4.pred ~ time.d, data = s9, col = 'red')
lines(cvCH4.pred2 ~ time.d, data = s9, col = 'blue')

# Drop (exclude) lag phase
mod3 <- fitFOM(s9, n.pool = 2, time.name = 'time.d', resp.name = 'cvCH4', lag.phase = TRUE)
mod3$summ
s9$cvCH4.pred3 <- mod3$pred

lines(cvCH4.pred3 ~ time.d, data = s9, col = 'darkgreen')

}

%% Add one or more standard keywords, see file "KEYWORDS" in the
%% R documentation directory.
\keyword{regression}
\concept{biogas}
