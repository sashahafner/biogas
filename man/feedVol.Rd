\name{feedVol}
\alias{feedVol}
\docType{data}
\title{
  Biogas Volume from Batch Reactors
}
\description{
  Cumulative standardized measurements of biogas volume from 12 batch bottles, organized in a \dQuote{wide} format.
}
\usage{data("feedVol")}
\format{
  A data frame with 44 observations on the following 13 variables:
  \describe{
    \item{\code{time.d}}{elapsed time of volume measurements in days, a numeric vector.}
    \item{\code{1}}{cumulative volume of biogas removed at \code{time} from bottle \dQuote{1} in mL, a numeric vector.}
    \item{\code{2}}{cumulative volume of biogas removed at \code{time} from bottle \dQuote{2} in mL, a numeric vector.}
    \item{\code{3}}{cumulative volume of biogas removed at \code{time} from bottle \dQuote{3} in mL, a numeric vector.}
    \item{\code{4}}{cumulative volume of biogas removed at \code{time} from bottle \dQuote{4} in mL, a numeric vector.}
    \item{\code{5}}{cumulative volume of biogas removed at \code{time} from bottle \dQuote{5} in mL, a numeric vector.}
    \item{\code{6}}{cumulative volume of biogas removed at \code{time} from bottle \dQuote{6} in mL, a numeric vector.}
    \item{\code{7}}{cumulative volume of biogas removed at \code{time} from bottle \dQuote{7} in mL, a numeric vector.}
    \item{\code{8}}{cumulative volume of biogas removed at \code{time} from bottle \dQuote{8} in mL, a numeric vector.}
    \item{\code{9}}{cumulative volume of biogas removed at \code{time} from bottle \dQuote{9} in mL, a numeric vector.}
    \item{\code{10}}{cumulative volume of biogas removed at \code{time} from bottle \dQuote{10} in mL, a numeric vector.}
    \item{\code{11}}{cumulative volume of biogas removed at \code{time} from bottle \dQuote{11} in mL, a numeric vector.}
    \item{\code{12}}{cumulative volume of biogas removed at \code{time} from bottle \dQuote{12} in mL, a numeric vector.}
  }
}
\details{
  These data are meant to be example data for \code{\link{cumBgVol}}.
Substrate was animal feed ingredients.
  
  Biogas productions was measured using the online volumtric AMPTS II method.
Data in \code{feedSetup} and \code{feedVol} are from the same bottles.
}
\source{
  Sasha D. Hafner. Data were originally collected by Soeren Weinrich and others at DBFZ in Leipzig Germany.
}
\examples{
data(feedVol)
}
\keyword{datasets}
\concept{biogas}