\name{calcCOD}
\alias{calcCOD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calculate Oxygen Demand
}
\description{
  \code{calcCOD} is used to calculate the oxygen demand (\dQuote{calculated oxygen demand}, or COD' as described by Rittmann and McCarty (2001)) of a compound.
}
\usage{
calcCOD(form)
}
\arguments{
  \item{form}{
    a chemical formula, as a character vector, e.g., \code{"C6H12O6"} for glucose or \code{"CH3COOH"} for acetic acid, or \code{c("C6H12O6", "CH3COOH")} for both at once.
    Not case-sensitive for single letter elements (see \sQuote{Details}).
}
}
\details{
  Based on Eqs. (2.2) and (2.3) in Rittmann and McCarty (2001) (p 128), but using molar mass calculated with \code{\link{molMass}}.
  \code{calcCOD} is not case-sensitive as long as all elements have single letter abbreviations.
  The function is vectorized.
}
\value{
  A numeric vector with length equal to length of \code{form} with COD' in g of oxygen per g of compound.
}
\references{
  Rittmann, B., McCarty, P. 2001 \emph{Environmental Biotechnology}. McGraw-Hill.
}
\author{
  Sasha D. Hafner and Charlotte Rennuit
}

\seealso{
  \code{\link{molMass}}, 
  \code{\link{predBg}}
}
\examples{
  calcCOD("C6H12O6")

  calcCOD("CH3COOH")

  calcCOD("CH3CH2OH")

  calcCOD("CH4")

  calcCOD("ch4")

  calcCOD(c("C6H12O6", "CH3COOH", "CH3CH2OH"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{chemical oxygen demand}
\concept{biogas}
