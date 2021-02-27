# Estimate air density

airDens <- function(
  temp,
  pres,
  rh = 0,
  unit.temp = getOption('unit.temp', 'C'),
  unit.pres = getOption('unit.pres', 'atm'),
  xdryair = c(N2 = 0.7810, O2 = 0.2095, Ar = 0.0092, CO2 = 0.0003), # From Table 2 in Lemmon et al. 2000
  tp.message = TRUE
) {

  # NTS: Needs some checks
  #checkArgClassValue(gas, expected.values = names(vol.mol))

  pres.pa <- unitConvert(x = pres, unit = unit.pres, to = 'Pa')
  temp.k <- unitConvert(x = temp, unit = unit.temp, to = 'K')

  if(tp.message) message('You specified an air pressure of ', pres, ' ', unit.pres, ' and temperature of ', temp, ' ', unit.temp, '.')

  # Water vapor pressure
  pH2O <- rh*watVap(temp.k = temp.k, pres.pa = pres.pa)
  xH2O <- pH2O / pres.pa

  xair <- c((1 - xH2O) * xdryair, H2O = xH2O)

  # Density at 273.15 K and 1 atm
  dens.std <- gasDens(xN2 = xair[['N2']], xO2 = xair[['O2']], xAr = xair[['Ar']], xH2O = xair[['H2O']])

  # Density at specified temperature and pressure
  dens <- 273.15 / temp.k * pres.pa / 101325 * dens.std

  return(dens)

}


