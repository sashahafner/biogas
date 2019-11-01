# Function for calculating biogas composition from gas density
# Jacob R. Mortensen and Sasha Hafner

# NTS: do we need two pres arguments, for grav calcs and (mH2O) and vol std?

GDComp <- function(
  mass,     # Mass loss in reactor
  vol,      # Standardized (dry, 1 atm, 0C) biogas volume
  temp,     # Headspace temperature at time of venting in unit.pres (grav)
  pres,     # Pressure of biogas in headspace at time of venting unit.pres (grav)
  vol.hs = NULL,   # Headspace volume in bottle (mL)
  headcomp = 'N2', 
  temp.init = NULL,
  pres.init = NULL,
  unit.temp = getOption('unit.temp', 'C'),   # Optional unit for temperature
  unit.pres = getOption('unit.pres', 'atm')  # Optional unit for pressure
) {
  
  # Check arguments~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # NTS: are all included? (I think so)
  checkArgClassValue(mass, c('integer', 'numeric'), expected.range = c(0, Inf))
  checkArgClassValue(vol, c('integer', 'numeric'))
  checkArgClassValue(temp, c('integer', 'numeric'))
  checkArgClassValue(pres, c('integer', 'numeric'))
  checkArgClassValue(vol.hs, c('integer', 'numeric', 'NULL'))
  checkArgClassValue(temp.init, c('integer', 'numeric', 'NULL'))
  checkArgClassValue(pres.init, c('integer', 'numeric', 'NULL'))
  checkArgClassValue(unit.temp, c('character'))
  checkArgClassValue(unit.pres, c('character'))

  # Unit conversion
  # NTS: are these needed? Can't we just use stdVol() with input arguments as in cumBg()? This applies to other functions as well. . .
  pres.pa <- unitConvert(x = pres, unit = unit.pres, to = 'Pa')
  temp.k <- unitConvert(x = temp, unit = unit.temp, to = 'K')
  if(!is.null(temp.init)) temp.init.k <- unitConvert(x = temp.init, unit = unit.temp, to = 'K')
  if(!is.null(pres.init)) pres.init.pa <- unitConvert(x = pres.init, unit = unit.pres, to = 'Pa')
    
  # Set constants~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set relative humidity - assumme 100 % humidity. Could be made as an argument..
  rh <- 1
    
  # Set biogas molar volume to 22300 mL/mol since difference between CO2 and CH4 is small
  mvBg <- 22300
    
  # Set density of headspace gas (g/ml) at 1 atm and 0C
  dhs <- gasDens(comp = headcomp)
     
  # Main calculations~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate water vapor mass
  pH2O <- rh*watVap(temp.k = temp.k)
  mH2O <- (molMass('H2O')*pH2O)/((pres.pa - pH2O)*mvBg)
  
  # Correct mass loss and measured volume for headspace flushing gas 
  # Note: assumes all flushing gas has been removed and headspace contains only biogas at fixed composition under same temperature and pressure as vol
  if(!is.null(vol.hs)) {
    message('Initial headspace gas (flushing gas) correction included.')

    vol.hs <- stdVol(vol.hs, temp = temp.init.k, pres = pres.init.pa, rh = 0, temp.std = 273.15, pres.std = 101325, unit.pres = 'Pa', unit.temp = 'K', std.message = FALSE)
    mass <- mass - (vol.hs * dhs)

    vol <- vol - vol.hs
  }

  # Dry biogas density
  db <- mass/vol - mH2O
  
  # Molar mass of biogas
  mmb <- db * mvBg
  
  # Mole fraction of CH4 in biogas
  xCH4 <- (molMass('CO2') - mmb) / (molMass('CO2') - molMass('CH4'))

  return(xCH4) 
}
