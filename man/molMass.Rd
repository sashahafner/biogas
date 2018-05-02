\name{molMass}
\alias{molMass}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calculate Molar Mass of Compound
}
\description{
  \code{molMass} calculates the molar mass of any organic compound (and some salts) based on its chemical formula.
}
\usage{
molMass(form)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{form}{a chemical formula, as a character vector, e.g., \code{"C6H12O6"} for glucose or \code{"CH3COOH"} for acetic acid, or \code{c("C6H12O6", "CH3COOH")} for both at once. 
}
}
\details{
  Standard atomic weights are from CIAAW, and were rounded to three or more digits (with the exception of Li) depending on the range of the reported interval for \dQuote{normal materials}.
  In general \code{form} should follow capitalization rules for elements (i.e., first letter capitalized, second lower-case). 
  However, if \code{form} contains single-letter elements only, it is possible to use lower-case letters for all elements (but for clarity this is not recommended).
  The function is vectorized.
}
\value{
  Numeric vector with length equal to length of \code{form} with molar mass (g/mol)
}
\references{
  CIAAW <http://www.ciaaw.org/atomic-weights.htm>
}
\author{
  Charlotte Rennuit and Sasha D. Hafner
}

\seealso{
  \code{\link{calcCOD}}
}
\examples{
  molMass("C6H12O6")

  molMass("CH3COOH")

  molMass("CH3CH2OH")

  molMass("CH4")

  molMass("ch4")

  molMass(c("C6H12O6", "CH3COOH", "CH3CH2OH"))

  # Case-sentitive for two letter elements
  molMass("NaHCO3")

  # Complex formulas OK
  molMass("H3C(CH2)5COOH")

  molMass("(C6H12O6)0.24999 (H3COOH)0.75001")

  # Database is quite complete 
  molMass('CdSiO3')

  # Use care for, e.g., hydrates
  molMass('FeSO4(H2O)7')

}
% Add one or more standard keywords, see file "KEYWORDS" in the
% R documentation directory.
\concept{biogas}
\concept{chemistry}
