# General function for bottle mass loss biogas leakage
# Camilla G. Justesen

massLoss <- function(
  # Main arguments 
  dat,           # Data frame
  time.name,     # Name of time column (for sorting only)
  m.pre.name,    # Name of column holding the mass before venting 
  m.post.name,   # Name of column holding the mass after venting 
  id.name        # Name of id to group by 
  ) {
  
  # Check arguments
  # NTS: add checks later

  # Main calculations~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Sort data frame
  dat <- dat[order(dat[, id.name], dat[, time.name]), ]
  # make if loops for if the substrate changes 

  # Loop through bottles
  for(i in unique(dat[, id.name])) {
    which.id <- which(dat[, id.name]==i)
    n.id <- length(which.id)

    dat[which.id, 'mass.tot']  <- c(0, -diff(dat[which.id, m.post.name]))
    dat[which.id, 'mass.vent'] <- dat[which.id, m.pre.name] - dat[which.id, m.post.name]
    dat[which.id, 'mass.leak'] <- c(0, dat[which.id, m.post.name][-n.id] - dat[which.id, m.pre.name][-1])

    # Calculation of cummulative mass loss due to leakage and by venting
    dat[which.id, 'cmass.tot'] <- cumsum(dat[which.id, 'mass.tot'])
    dat[which.id, 'cmass.leak'] <- cumsum(dat[which.id, 'mass.leak'])
    dat[which.id, 'cmass.vent'] <- cumsum(dat[which.id, 'mass.vent'])

  }


  # NTS: do we need this?
  ## Calculate the interval
  #dat$interval = dat[, time.name] - lag(dat[, time.name])
  
  # Also needs calculation of leakage in volume units - also in pressure?
  # For adding the volumetric leakage eventually look at the unit conversion, standardize total gas volumes, etc. also, from cumBg.R

  return(dat)
}
