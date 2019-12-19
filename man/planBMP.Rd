\name{planBMP}
\alias{planBMP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Claculate Inoculum and Substrate Mass for BMP Experiments 
}
\description{
  \code{planBMP} assists in the design of BMP experiments. 
  It can be used to determine inoculum and substrate masses based on inoculum-to-substrate ratio and volatile solids concentrations, or to calculate inoculum-to-substrate ratio based on masses.
}
\usage{
planBMP(vs.inoc, vs.sub, isr = NA, m.inoc = NA, m.sub = NA, 
        m.tot = NA, m.vs.sub = vs.sub * m.sub, digits = 3, warn = TRUE,
        nice = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vs.inoc}{
      volatile solids (VS) concentration of inoculum (g/g = g VS per g inoculum).
      Required.
      Numeric vector.
}

  \item{vs.sub}{
      volatile solids (VS) concentration of substrate (g/g = g VS per g substrate).
      Required.
      Numeric vector.
}

  \item{isr}{
      inoculum-to-substrate ratio, VS mass basis.
      Optional.
      Numeric vector.
}

  \item{m.inoc}{
      total mass of inoculum (g).
      Optional.
      Numeric vector.
}

  \item{m.sub}{
      total mass of substrate (g).
      Optional.
      Numeric vector.
}

  \item{m.tot}{
      total mass of mixture (inoculum plus substrate) (g).
      Optional.
      Numeric vector.
}

  \item{m.vs.sub}{
      VS mass of substrate (g).
      Optional.
      Numeric vector.
}

  \item{digits}{
      number of significant digits to display in output.
      Default of 3.
      Integer vector with length 1.
}

  \item{warn}{
      control whether warnings are displayed.
      Default of TRUE.
      Logical vector with length 1.
}

  \item{nice}{
      control whether output is formatted to look nice and make reading easier.
      Default of TRUE.
      Only applied for non-vectorized (length 1) calls.
      Logical vector with length 1.
}

}
\details{
    BMP experiments should be designed giving consideration to the inoculum-to-substrate ratio (ISR), the substrate VS mass, and the mixture VS concentration. 
    This function calculates inoculum and substrate masses based on VS concentrations and ISR, along with either total mixture mass or substrate VS mass.
    Alternatively, it can be used to calculate ISR if the masses have been selected.
    Warnings are based on the guidelines of Holliger et al. (2016).
}
\value{
  A named numeric vector, or (if any of the first 7 input arguments have a length > 1, i.e., a vectorized call), a data frame.
  Names and interpretation are identical to the first 7 input arguments, and also include:
 \item{vs.mix}{VS concentration in mixture (g/g)}
 \item{m.vs.tot}{total VS mass in mixture (g)}
  For non-vectorized calls, the results is returned invisibly and a easy-to-read summary is printed (see \code{nice} argument).
}
\references{
Holliger, C., Alves, M., Andrade, D., Angelidaki, I., Astals, S., Baier, U., Bougrier, C., Buffiere, P., Carbella, M., de Wilde, V., Ebertseder, F., Fernandez, B., Ficara, E., Fotidis, I., Frigon, J.-C., Fruteau de Laclos, H., S. M. Ghasimi, D., Hack, G., Hartel, M., Heerenklage, J., Sarvari Horvath, I., Jenicek, P., Koch, K., Krautwald, J., Lizasoain, J., Liu, J., Mosberger, L., Nistor, M., Oechsner, H., Oliveira, J.V., Paterson, M., Pauss, A., Pommier, S., Porqueddu, I., Raposo, F., Ribeiro, T., Rusch Pfund, F., Stromberg, S., Torrijos, M., van Eekert, M., van Lier, J., Wedwitschka, H., Wierinck, I., 2016. Towards a standardization of biomethane potential tests. \emph{Water Science and Technology} \bold{74}, 2515-2522.
}

\note{
    Calculations used in this function are trivial, and they could also be done with a spreadsheet or even pencil and paper.
    The advantage here is ease and some flexibility. 
    In addition to ISR and the other parameters included in this function, expected biogas production rate and bottle headspace volume are important, depending on the method.
    For more details, see Holliger et al. (2016).
}
\author{
  Sasha D. Hafner, based on suggestion by Konrad Koch
}

\seealso{
  \code{\link{calcBgVol}},
  \code{\link{calcBgMan}},
  \code{\link{calcBgGD}},
  \code{\link{cumBg}},
  \code{\link{summBg}},
  \code{\link{predBg}}
}
\examples{

  # Bottles are 500 mL, substrate is wastewater sludge.
  # Assume we want no more than 250 mL reacting volume (~250 g)
  # First try setting ISR and total mass.
  # VS concentrations: 0.02 g/g in inoculum, 0.07 g/g for substrate, ISR = 2.
  planBMP(vs.inoc = 0.02, vs.sub = 0.07, isr = 2, m.tot = 250)
  # Get 31 g substrate, 220 g inoculum.
  
  # After setup, we can check final values.
  planBMP(vs.inoc = 0.018, vs.sub = 0.072, m.sub = 32, m.inoc = 218)
  # We didn't quite meet our target in this case--next time use more inoculum to be sure
  
  # We can alternatively specify substrate VS mass
  planBMP(vs.inoc = 0.02, vs.sub = 0.07, isr = 2, m.vs.sub = 2)
  
  # Some options
  planBMP(vs.inoc = 0.02, vs.sub = 0.07, isr = 2, m.vs.sub = 2, nice = FALSE)
  # Perhaps we want to use three different ISRs
  planBMP(vs.inoc = 0.02, vs.sub = 0.07, isr = 2:4, m.vs.sub = 2, nice = FALSE)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\concept{biogas}
