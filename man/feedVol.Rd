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
  A data frame with 44 observations on the following 13 variables (note that all biogas volume columns, 2-13, are similar):
  \describe{
    \item{\code{time.d}}{elapsed time of volume measurements in days, a numeric vector.}
    \item{\code{1}}{ cumulative standardized volume of biogas produced at time given in \code{time} from bottle \dQuote{1} in mL, a numeric vector.}
    \item{\code{2}}{ volume of biogas from bottle \dQuote{2}.}
    \item{\code{3}}{ volume of biogas from bottle \dQuote{3}.}
    \item{\code{4}}{ volume of biogas from bottle \dQuote{4}.}
    \item{\code{5}}{ volume of biogas from bottle \dQuote{5}.}
    \item{\code{6}}{ volume of biogas from bottle \dQuote{6}.}
    \item{\code{7}}{ volume of biogas from bottle \dQuote{7}.}
    \item{\code{8}}{ volume of biogas from bottle \dQuote{8}.}
    \item{\code{9}}{ volume of biogas from bottle \dQuote{9}.}
    \item{\code{10}}{volume of biogas from bottle \dQuote{10}.}
    \item{\code{11}}{volume of biogas from bottle \dQuote{11}.}
    \item{\code{12}}{volume of biogas from bottle \dQuote{12}.}
  }
}
\details{
  These data are meant to be example data for \code{\link{calcBgVol}} and \code{\link{summBg}}.
  Inoculum was digestate from a stable laboratory reactor (400 L) operated at mesophilic temperatures (38 degrees C) and a retention time of ca. 98 days
  Substrates were cellulose, wheat straw (WS), and animal feed ingredients (FI).
  Additinally, 1 mL each of a vitamin mixture and trace element solution was added to each BMP bottle.
  BMP tests were operated with a total volume of ca. 400 mL at 38 degrees C. 
  
  Biogas productions was measured using the online volumtric AMPTS II method.
  Carbon dioxide was produced using an alkaline trap, so biogas volume is equal to methane volume.

  Data in \code{feedSetup} and \code{feedVol} are from the same bottles.
}
\source{
  Data were originally collected by Soeren Weinrich and others at DBFZ in Leipzig Germany.
}
\examples{
data(feedVol)
}
\keyword{datasets}
\concept{biogas}
