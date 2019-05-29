# Calculate pure gas density for headspace correction in gravimetric and GD methods
# S. Hafner

# Options: N2, CO2, CH4, 1N2:

gasDens <- function(
  comp = 'N2'
) {

  # Get ratio of gases
  comps <- strsplit(comp, ':')[[1]]
  # If leading 1 is missing, add it
  comps <- gsub('^([[:alpha:]])', '1\\1', comps)
  rat <- as.numeric(gsub('[[:alpha:]]+[[:digit:]]', '', comps))
  rat <- rat/sum(rat)
  names(rat) <- gsub('^[[:digit:]]+', '', comps)
  names(rat) <- gsub('^\\.', '', names(rat))
  names(rat) <- gsub('^[[:digit:]]+', '', names(rat))

  vol <- sum(rat * vol.mol[names(rat)])
  mass <- sum(rat * molMass(names(rat))) 
  dens <- mass/vol
  
  return(dens)

}



