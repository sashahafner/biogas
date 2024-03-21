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
firFOM(dat, n.pool = 1, time.name = 'time.d', resp.name = 'cvCH4',
       fit.to = 'yield', method = 'LM', abs.err = FALSE, trans = TRUE,
       init = if (n.pool == 1) c(B = 'yield', k = 0.5) else c(B = 'yield', f = 0.5, k1 = 0.01, k2 = 0.5), 
       fixed = NULL, fit.last = FALSE, lower = NULL, upper = NULL, lag.phase = FALSE)
}

%- maybe also "usage" for other objects documented here.
\arguments{
    \item{dat}{
      a data frame with a column for elapsed time and cumulative response variable.
  }
}
\details{
  Use for fitting first-order model. See examples.
  
}

\value{
  A list.
}

%\references{
%}

\author{
  Sasha D. Hafner
}

%\seealso{
%}

\examples{ 
# Wide data structure
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

plot(cvCH4 ~ time.d, data = s9, type = 'o')
lines(cvCH4.pred ~ time.d, data = s9, col = 'red')

# Try 2 pools
mod2 <- fitFOM(s9, n.pool = 2, time.name = 'time.d', resp.name = 'cvCH4')
mod2$summ
s9$cvCH4.pred2 <- mod2$pred

lines(cvCH4.pred2 ~ time.d, data = s9, col = 'blue')

# Drop lag phase
mod3 <- fitFOM(s9, n.pool = 2, time.name = 'time.d', resp.name = 'cvCH4', lag.phase = TRUE)
mod3$summ
s9$cvCH4.pred3 <- mod3$pred

lines(cvCH4.pred3 ~ time.d, data = s9, col = 'green')

}

%% Add one or more standard keywords, see file "KEYWORDS" in the
%% R documentation directory.
\keyword{chron}
\keyword{manip}
\concept{biogas}
