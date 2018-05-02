\name{biogas-package}
\alias{biogas-package}
\alias{biogas}
\docType{package}
\title{
Process Biogas Data and Predict Biogas Production
}
\description{
Functions for working with biogas data. Both low- and high-level functions are included for carrying out common tasks for processing and analysis of biogas and related data. Molar mass and calculated oxygen demand (COD') can be determined from a chemical formula. Measured gas volume can be corrected for water vapor and to (possibly user-defined) standard temperature and pressure. Gas composition, cumulative production, or other variables can be interpolated to a specified time. Cumulative biogas and methane production (and rates) can be calculated from individual biogas volume (or reactor mass) and biogas composition measurements for any number of reactors. With cumulative methane production data and data on reactor contents, biochemical methane potential (BMP) can be calculated and summarised, including subtraction of the inoculum contribution and normalisation by substrate mass. Cumulative production and production rates can be summarised in several different ways (e.g., omitting normalisation) using the same function. Lastly, biogas and methane production can be predicted from substrate composition and additional, optional data.
}
\details{
\tabular{ll}{
Package: \tab biogas\cr
Type: \tab Package\cr
Version: \tab 1.8.1\cr
Date: \tab 2017-08-12\cr
License: \tab GPL-2\cr
}
    The \code{biogas} package provides several low-level functions for working with biogas measurements and related data.
    Use the function \code{\link{stdVol}} to correct biogas volumes to \dQuote{standard} temperature and pressure, and dry conditions.
    For interpolating biogas composition or cumulative production, use \code{\link{interp}}.
    The function \code{\link{molMass}} and \code{\link{calcCOD}} can be used to calculate molar mass and chemical oxygen demand based on a chemical formula.
    And for gravimetric measurement of biogas production, there is \code{\link{mass2vol}} and \code{\link{vol2mass}}.
    To convert gas volume to moles, use \code{\link{vol2mol}}.

    Additionally, there are two high-level functions in the \code{biogas} package that simplify processing of biogas measurements. 
    \code{\link{cumBg}} calculates cumulative biogas and methane production from separate, indvidual observations of biogas volume (or reactor mass) and biogas composition for any number of batch reactors.
    Interpolation is automatically applied as needed. 
    And \code{\link{summBg}} summarises cumulative methane production by providing cumulative values for a specified time.
    \code{\link{summBg}} can calculate biochemical methane potential (BMP) from measurements by subtracting the contribution of an inoculum, and normalising results by substrate mass.
    Time can be defined explicitely (e.g., 60 days) or based on methane production rates.

    Lastly, \code{\link{predBg}} can be used to predict biogas production and composition based on substrate composition, along with degradability and other optional parameters.
    \code{\link{predBg}} will predict biogas and methane volume (and mass) based on an empirical formula or \dQuote{macromolecule}-based composition.
    If other parameters are provided (e.g., degradability, microbial substrate partitioning, solution pH, or substrate concentration), additional predictions can be made, e.g., biogas composition and microbial nitrogen requirement.
}
\author{
Sasha D. Hafner, Charlotte Rennuit, Jin Mi Triolo, and Ali Heidarzadeh Vazifehkhoran.

Maintainer: Sasha D. Hafner <sasha.hafner@eng.au.dk>
}
\references{
  Hafner, S.D., Rennuit, C., Triolo, J.M., Richards, B.K. 2015. Validation of a simple gravimetric method for measuring biogas production in laboratory experiments. \emph{Biomass and Bioenergy} \bold{83}, 297-301.

  Richards, B.K., Cummings, R.J., White, T.E., Jewell, W.J. 1991. Methods for kinetic analysis of methane fermentation in high solids biomass digesters. \emph{Biomass and Bioenergy} \bold{1}, 65-73.

  Rittmann, B., McCarty, P. 2001 \emph{Environmental Biotechnology}. McGraw-Hill.
}
\keyword{package}
\examples{
# Calculate BMP with data from a laboratory BMP test, and compare results for 
# cellulose to the expected value

# First load example data sets
data("vol")
data("comp")
data("setup")

# vol has measured biogas volumes
head(vol)

# comp has measured biogas composition
head(comp)

# setup has information on batch reactor content
head(setup)

# Calculate cumulative biogas and methane production volumes at dry conditions and 
# 0 degrees C and 1.0 atm
cum.vol <- cumBg(vol, comp = comp, time.name = "days", temp = 35, 
                 pres = 1, extrap = TRUE)

head(cum.vol)

dim(cum.vol)

cum.vol

# Now calculate BMP (60 days) with summBg()
# Interpolation is used if needed, inoculum contribution 
# is calculated and subtracted out, and results are 
# normalised by substrate VS mass
BMP <- summBg(cum.vol, setup = setup, time.name = "days", 
              inoc.name = "inoc", inoc.m.name = "minoc", 
              norm.name = "mvs.sub", when = 60)
BMP

# More details with show.obs = TRUE
summBg(cum.vol, setup = setup, time.name = "days", 
       inoc.name = "inoc", inoc.m.name = "minoc", 
       norm.name = "mvs.sub", when = 60, 
       show.obs = TRUE)

# How do results compare to theoretical methane potential for 
# cellulose?
predBg("C6H10O5")

# Set aside 10% of cellulose for microbial biomass production
predBg("C6H10O5", fs = 0.1)

# Both are close to measured values

}
\keyword{univar}
\concept{biogas}
