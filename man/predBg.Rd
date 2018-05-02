\name{predBg}
\alias{predBg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Predict Biogas Production
}
\description{
  \code{predBg} predicts biogas and methane production based on composition and other, optional, details.
}
\usage{
predBg(form = NULL, mass = 1, mol = NULL, fs = 0, fd = 1, 
       mcomp = NULL, COD = NULL, conc.sub = NULL, pH = NULL, 
       temp = NULL, mu = 0.1, shortform = NULL, value = "CH4")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{form}{
    (optional) a chemical formula for the substrate, as a character vector of length one, e.g., \code{"C6H12O6"} for glucose or \code{"CH3COOH"} for acetic acid.
}
  \item{mass}{
    mass of substrate present, in g.
    A numeric vector.
}
  \item{mol}{
    (optional) moles of substrate present.
    A numeric vector.
}
  \item{fs}{
    fraction of substrate used for microbial biomass production (\code{0<=fs<=1}).
    See "Details".
    A numeric vector.
}
  \item{fd}{
    fraction of substrate that is degradable (\code{0<=fd<=1}).
    See "Details".
    A numeric vector.
}
  \item{mcomp}{
    (optional) \dQuote{macromolecule}-based composition of the substrate.
    A named numeric vector with relative masses of macromolecular groups or any chemical. 
    Options for macromolecular groups include: \code{VFA} (volatile fatty acids, VFAs), \code{protein}, \code{carbohydrate}, \code{lipid}, and \code{lignin}.
    An empirical \code{form} will be calculated from fixed chemical formulas.
    To see the available options, use \code{biogas:::std.forms}.
}
  \item{COD}{
    (optional) chemical oxygen demand (COD) of the substrate (g oxygen).
    If provided, \code{mass} will be ignored.
    A numeric vector. 
}
  \item{conc.sub}{
    (optional) concentration of the substrate relative to water, as g substrate per kg water.
    Used only for carbon dioxide partitioning.
    A numeric vector.
}
  \item{pH}{
    (optional) pH of the solution.
    Used only for carbon dioxide partitioning.
    A numeric vector.
}
  \item{temp}{
    (optional) temperature of the system in degrees C.
    Used only for carbon dioxide partitioning.
    A numeric vector.
}
  \item{mu}{
    (optional) ionic strength of the solution.
    Used only for carbon dioxide partitioning.
    A numeric vector.
}
  \item{shortform}{
    should formula from \code{form} or \code{mcomp} arguments be shortened?
    Default is \code{FALSE} if \code{form} is used, \code{TRUE} if \code{mcomp} is used.
}
  \item{value}{
    what should be returned?
    Four options are currently available.
    \code{"CH4"} (default) returns standardised methane volume only.
    \code{"all"} returns all available additional results.
    \code{"reactionn"} returns a numeric reaction (names are chemical species).
    \code{"reactionc"} returns a text reaction.
    Length-one character vector.
}
}
\details{
  \code{predBg} is a flexible function that returns different details depending on the data provided. 
  Calculations can be based on \code{form}, \code{COD}, or \code{mcomp} (at least one of these is required).
  See \sQuote{Examples} for more information.

  In its simplest usage, \code{predBg} calculates theoretical biochemical methane potential (theoretical BMP).
  %Alternatively, it can calculate the methane equivalence of chemical oxygen demand (COD).
  With more parameters, it can also predict carbon dioxide partitioning, total biogas production and composition, as well as microbial biomass production and nitrogen requirement.
  Stoichiometry is based on Eq. 13.5 in Rittmann and McCarty (2001).
  Partitioning of carbon dioxide is based on an equilibrium speciation model using temperature-dependent parameters (Henry's law constant and dissociation constants) based on Hafner et al. (2012).
  \code{predBg} is vectorized for all arguments except \code{mcomp}, and will recycle argument elements as needed.
}
\value{
  Standardised volume (at 0 degrees C and 1 atmosphere) of methane produced in mL (for \code{value = "CH4"}), or a data frame with some of these columns (depending on provided arguments):
  \item{form}{empirical chemical formula of substrate, typically from the input argument}
  \item{mass}{substrate mass in g, typically from the input argument}
  \item{mol.mass}{substrate molar mass in g/mol}
  \item{moles}{moles of subtrate}
  \item{COD}{total calculated oxygen demand (COD') based on \code{form} (or echoed chemical oxygen demand (COD)) of substrate in g oxygen}
  \item{fs}{\code{fs} argument echoed}
  \item{fe}{Rittmann and McCarty's fe (\code{fe = 1 - fs})}
  \item{fd}{\code{fd} argument echoed}
  \item{conc.sub}{\code{conc.sub} argument echoed}
  \item{temp}{\code{temp} argument echoed}
  \item{pH}{\code{pH} argument echoed}
  \item{hydro}{hydrolytic water consumption (g water)}
  \item{fCH4}{moles methane producted divided by the sum of methane and carbon dioxide moles. Equal to \code{xCH4} in biogas if carbon dioxide (and related aqueous species) in solution are negligible}
  \item{xCH4}{mole fraction of methane in dry biogas}
  \item{vCH4}{standardised volume (dry, 0 degrees C, 1.0 atm) of methane produced in mL}
  \item{mCH4}{mass of methane produced in g}
  \item{mCO2}{mass of carbon dioxide produced in g (including both biogas and inorganic carbon in solution)}
  \item{mCO2Bg}{mass of carbon dioxide in biogas in g}
  \item{mCO2.sol}{mass of inorganic carbon in solution in g}
  \item{cTIC}{concentration of inorganic carbon in solution in mol/kg (per kg water)}
  \item{m.bio}{mass of microbial biomass produced, VS only, in g}
  \item{N.req}{nitrogen required for production of required microbial biomass in g of N. Negative value indicates mineralization}
}
\references{
  Hafner, S.D. 2007 \emph{Ammonia Speciation in Anaerobic Digesters}. PhD dissertation, Cornell University.

  Hafner, S.D., Montes, F., Rotz, C.A. 2012 The role of carbon dioxide in emission of ammonia from manure. \emph{Atmospheric Environment} \bold{66}, 63-71.

  Moller, H.B., Sommer, S.G., Ahring, B.K. 2004 Methane productivity of manure, straw and solid fractions of manure. \emph{Biomass and Bioenergy} \bold{26}, 485-495.  
  
  Rittmann, B., McCarty, P. 2001 \emph{Environmental Biotechnology}. McGraw-Hill.

  Triolo, J.M., Sommer, S.G., Moller, H.B., Weisbjerg, M.R., Jiang, X.Y. 2011 A new algorithm to characterize biodegradability of biomass during anaerobic digestion: Influence of lignin concentration on methane production potential. \emph{Bioresource Technology} \bold{102}, 9395-9402.
}
\author{
  Sasha D. Hafner and Charlotte Rennuit
}
\note{
  Predictions will only be as good as the parameter values provided, and maybe not even that good.
  \code{fs} should be interpreted as $f_s$ in Ritttmann and McCarty (2001), i.e., the effective value after decay of microbial biomass, and not $f_s^0$.
  The original reference (Section 2.3 and Eq. (3.33) in Rittmann and McCarty (2001)) and the \code{predBg} vignette provide more details.
  Partitioning of carbon dioxide is based on equilibrium between all biogas produced and the solution, and represents a continuous reactor running at steady-state.
}

\seealso{
  \code{\link{calcCOD}},
  \code{\link{molMass}}
}
\examples{
  # BMP of cellulose in mL CH4/g
  predBg("C6H10O5")

  # How much is produced in a real reactor? Assume 10% goes to 
  # biomass production
  predBg("C6H10O5", fs = 0.1)

  # And substrate is 80% degradable
  predBg("C6H10O5", fs = 0.1, fd = 0.8)

  # More detailed results
  predBg("C6H10O5", value = "all")

  # Given a substrate with a COD of 1.4 g
  predBg(COD = 1.4)

  # But CH4 production is reduced if substrate is 80% degradable 
  # and 10% goes to cell synthesis
  predBg(COD = 1.4, fd = 0.8, fs = 0.1)

  # Can use macromolecule composition, e.g., for pig manure
  predBg(mcomp = c(vfa = 0.2, protein = 0.25, carbohydrate = 0.32, 
                   lipid = 0.15, lignin = 0.08), 
         value = "all")

  # Function expects sum(mcomp) == 1 but this is not required
  # But unless mass is set, mass is taken as sum(mcomp)
  predBg(mcomp = c(vfa = 20, protein = 25, carbohydrate = 32, 
                   lipid = 15, lignin = 8), 
         value = "all")

  # Specify mass if this is not the case
  predBg(mcomp = c(vfa = 20, protein = 25, carbohydrate = 32, 
                   lipid = 15, lignin = 8), 
         mass = 1, value = "all")

  # Can mix formulas and macromolecular groups in mcomp
  predBg(mcomp = c(C6H12O6 = 0.5, protein = 0.5))

  # For CO2 partitioning, must provide conc.sub, pH, and temp 
  # cattle manure example from Hafner (2007)
  predBg("C13H20O11N", mass = 1, fs = 0.1, fd = 0.56, 
         conc.sub = 85, pH = 7.7, temp = 35, value = "all")

  # Or, mix of waste paper and waste vegetable oil from vignette
  predBg(mcomp = c(C6H10O5 = 5/6, C54H100O7 = 1/6), mass = 1, 
	 fd = 0.8, fs = 0.1, conc.sub = 50, pH = 7.5, temp = 35, 
	 value = "all")

  # Note that form can also be used for mixtures, but here it is 
  # molar ratios that are specified
  predBg('(C6H10O5)5(C54H100O7)1)', mass = 1, 
	 fd = 0.8, fs = 0.1, conc.sub = 50, pH = 7.5, temp = 35, 
	 value = "all")
  
  # Function is vectorized for everything except mcomp, e.g., 
  # fs
  predBg("C6H10O5", fs = c(0, 0.05, 0.1), value = "all")

  # form
  predBg(c("C6H10O5", "C10H19O3N", "CH3CH2OH"), value = "all")
  
  # pH
  predBg("C13H20O11N", conc.sub = 85, fs = 0.1, fd = 0.56, 
         pH = 7+0:10/10, temp = 35, value = "all")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{biogas}
\concept{chemistry}
