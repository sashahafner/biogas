# Calculate pure gas density for gravimetric and GD methods
# S. Hafner

gasDens <- function(
  comp = NULL,
  xCH4 = 0,
  xCO2 = 0,
  xN2 = 0,
  xO2 = 0,
  xAr = 0,
  xH2O = 0,
  value = 'dens'
) {

  if (!is.null(comp)) {
    rat <- getGasRats(comp)
  } else {
    rat <- c(CH4 = xCH4, CO2 = xCO2, N2 = xN2, O2 = xO2, Ar = xAr, H2O = xH2O)
  }

  vol <- molVolMix(rat)
  mass <- molMassMix(rat)
  dens <- mass/vol
  
  if (value == 'dens') return(dens)
  if (value == 'all') return(c(dens = dens, mol.vol = vol, mol.mass = mass))

}

molVolMix <- function(rat) {
  return(sum(rat * vol.mol[names(rat)])/sum(rat))
}

molMassMix <- function(rat) {
  return(sum(rat * molMass(names(rat)))/sum(rat))
}

# Get ratio of gases from e.g. 50N2:20CO2:30CH4
# Returns named vector
getGasRats <- function(comp) {
  comps <- strsplit(comp, ':')[[1]]
  # If leading 1 is missing, add it
  comps <- gsub('^([[:alpha:]])', '1\\1', comps)
  rat <- as.numeric(gsub('[[:alpha:]]+[[:digit:]]', '', comps))
  rat <- rat/sum(rat)
  names(rat) <- gsub('^[[:digit:]]+', '', comps)
  names(rat) <- gsub('^\\.', '', names(rat))
  names(rat) <- gsub('^[[:digit:]]+', '', names(rat))
  return(rat)
}

