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

}

%% Add one or more standard keywords, see file "KEYWORDS" in the
%% R documentation directory.
\keyword{chron}
\keyword{manip}
\concept{biogas}
