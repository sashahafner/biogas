calcBgGrav <- function(
  # Main arguments
  dat,
  comp = NULL, # Leave NULL for wide and both combos
  temp = NULL,
  pres = NULL,
  data.struct = 'longcombo', # long, wide, longcombo, widecombo
  # Column names
  id.name = 'id',
  time.name = 'time',
  mass.name,             # Will be used for first dat column for data.struct = 'wide'
  xCH4.name = 'xCH4',    # Use for first comp col for data.struct = 'wide'
  xCO2.name = NULL,      # Only needed for cmethod = 'total'
  xN2.name = NULL,       # Only needed for cmethod = 'total'
  # Additional arguments 
  headspace = NULL,      # For cmethod = 'total' *or* headspace correction
  vol.hs.name = 'vol.hs',
  temp.init = NULL,      # For headpsace correction with cmethod = 'removed'
  pres.init = NULL,      # For headpsace correction with cmethod = 'removed'
  pres.resid = NULL,       # Only for cmethod = 'total'
  headcomp = 'N2',       # For headspace correction, can be any formula
  # Calculation method and other settings
  cmethod = 'removed',
  imethod = 'linear',
  extrap = FALSE,
  addt0 = TRUE,
  showt0 = TRUE,
  # Warnings and messages
  std.message = !quiet,
  check = TRUE,
  # Units and standard conditions
  temp.std = getOption('temp.std', as.numeric(NA)),
  pres.std = getOption('pres.std', as.numeric(NA)),
  unit.temp = getOption('unit.temp', 'C'),
  unit.pres = getOption('unit.pres', 'atm'),
  quiet = FALSE
){

  # Check arguments
  checkArgClassValue(dat, 'data.frame')
  checkArgClassValue(comp, c('data.frame', 'integer', 'numeric', 'NULL'))
  checkArgClassValue(temp, c('integer', 'numeric', 'character', 'NULL'))
  checkArgClassValue(pres, c('integer', 'numeric', 'character', 'NULL'))
  checkArgClassValue(data.struct, 'character', expected.values = c('long', 'wide', 'longcombo'))
  checkArgClassValue(id.name, 'character')
  checkArgClassValue(time.name, 'character')
  checkArgClassValue(mass.name, 'character')
  checkArgClassValue(xCH4.name, c('character', 'NULL'))
  checkArgClassValue(xCO2.name, c('character', 'NULL'))
  checkArgClassValue(xN2.name, c('character', 'NULL'))
  checkArgClassValue(headspace, c('data.frame', 'integer', 'numeric', 'NULL'))
  checkArgClassValue(vol.hs.name, 'character')
  checkArgClassValue(temp.init, c('integer', 'numeric', 'NULL'))
  checkArgClassValue(pres.init, c('integer', 'numeric', 'NULL'))
  checkArgClassValue(pres.resid, c('integer', 'numeric', 'character', 'NULL'))
  checkArgClassValue(headcomp, c('character', 'NULL'))
  checkArgClassValue(cmethod, 'character', expected.values = c('removed', 'total'))
  # Skip imethod, checked in interp
  checkArgClassValue(extrap, 'logical')
  checkArgClassValue(addt0, 'logical')
  checkArgClassValue(showt0, 'logical')
  checkArgClassValue(std.message, 'logical')
  checkArgClassValue(check, 'logical')
  checkArgClassValue(temp.std, c('integer', 'numeric'))
  checkArgClassValue(pres.std, c('integer', 'numeric'))
  checkArgClassValue(unit.temp, 'character')
  checkArgClassValue(unit.pres, 'character')

  # Hard-wire rh for now at least
  rh <- 1

  # Check of column names in argument data frames 
  # Also occurs in cumBgDataPrep() but some column names are only available here
  # comp needs id (time) xCH4, time optional
  if(!is.null(comp) && any(class(comp) == 'data.frame') && data.struct[1] == 'long') {
    if(any(missing.col <- !c(id.name, xCH4.name) %in% names(comp))){
      stop('Specified column(s) in comp data frame (', deparse(substitute(comp)), ') not found: ', paste(c(id.name, xCH4.name)[missing.col], collapse = ', '), '.')
    }
  }

  # dat (volume)
  if(data.struct %in% c('long', 'longcombo')) {
    if(any(!c(id.name, time.name, mass.name) %in% names(dat))){
      missing.col <- !c(id.name, time.name, mass.name) %in% names(dat)
      stop('Specified columns in dat data frame (', deparse(substitute(dat)), ') not found: ', paste(c(id.name, time.name, mass.name)[missing.col], collapse = ', '), '.')
    } 
  } else if(data.struct == 'wide') {
    if(any(!c(time.name, mass.name) %in% names(dat))){
      missing.col <- !c(time.name, mass.name) %in% names(dat)
      stop('Specified columns in dat data frame (', deparse(substitute(dat)), ') not found: ', paste(c(time.name, mass.name)[missing.col], collapse = ', '), '.')
    } 
  }

  # Check for headspace argument if it is needed
  if(is.null(headspace) && cmethod[1] == 'total') stop('cmethod is set to \"total\" but headspace argument is not provided.')

  # Check for other input errors
  if(!is.null(id.name[1]) && id.name %in% names(dat)) {
    if(any(is.na(dat[, id.name]))) {
      w <- which(is.na(dat[, id.name]))
      stop('Missing values in id.name column! See rows ', paste(w, collapse = ', '), '.')
    }
  }

  # Check cmethod
  if (cmethod == 'removed' && sum(is.null(xCO2.name), is.null(xN2.name)) < 2) {
    warning('You selected cmethod = \"removed\" but provided xCO2.name
         or xN2.name columns, which implies cmethod = \"total\".')
  }

  if (cmethod == 'total' && (is.null(xCO2.name) || is.null(xN2.name))) {
    stop('You selected cmethod = \"total\" but did not provide
         either xCO2.name or xN2.name columns, which means this
         method cannot be applied. Did you mean cmethod = \"removed\"?')
  }

  if (cmethod == 'total' && is.null(pres.resid)) {
    stop('For cmethod = \"total\" pres.resid must be given (column name or numeric value).') 
  }

  if (cmethod == 'total' && data.struct != 'longcombo') {
    warning('For cmethod = \"total\" data.struct = \"longcombo\" is only option and will be assumed here.') 
    data.struct <- 'longcombo'
  }

  # Data preparation (structuring and sorting)
  # Returns dat as data.struct = 'longcombo'
  dat <- cumBgDataPrep(dat = dat, dat.type = 'mass', dat.name = mass.name, 
                       comp.name = xCH4.name, id.name = id.name, 
                       time.name = time.name, data.struct = data.struct, comp = comp, 
                       have.comp = TRUE,
                       interval = FALSE, imethod = imethod, extrap = extrap, 
                       headspace = headspace, vol.hs.name = vol.hs.name, 
                       temp = temp, pres = pres, rh = rh,
                       temp.std = temp.std, pres.std = pres.std, unit.temp = unit.temp,
                       unit.pres = unit.pres, std.message = std.message, check = check)


  # Add temperature and pressure to dat if single numeric values were provided
  if(!is.null(temp)) {
    if(is.numeric(temp)) {
      dat[, 'temperature'] <- temp
      temp <- 'temperature'
    }
  }

  if(!is.null(pres)) {
    if(is.numeric(pres)) {
      dat[, 'pressure'] <- pres
      pres <- 'pressure' 
    } 
  }

  if(!is.null(pres.resid)) {
    if(is.numeric(pres.resid)) {
      dat[, 'pres.resid'] <- pres.resid
      pres.resid <- 'pres.resid' 
    } 
  }
  
  # For data.struct = 'wide', data and composition names are fixed, added manually in cumBgDataPrep()
  if(data.struct == 'wide') {
    vol.name <- 'vol'
    xCH4.name <- 'xCH4'
  }
 
  # Check for pressure and temperature--required, but default is NULL (for use of volumetric method without standardization) so must check here 
  if(is.null(temp)[1]) stop('temp argument missing but is required for gravimetric method.')
  if(is.null(pres)[1]) stop('pres argument missing but is required for gravimetric method.')
  # With longcombo separate comp data frame is not needed, only xCH4.name is needed in main data frame
  if(data.struct[1] == 'longcombo') {
    if(is.null(xCH4.name)[1]) stop('xCH4.name argument missing but is required for gravimetric method.')
  } else {
    if(is.null(comp)[1]) stop('comp argument missing but is required for gravimetric method.')
  }

  # Calculate mass loss
  dat <- massLoss(dat, time.name = time.name, m.pre.name = NULL, m.post.name = mass.name, id.name = id.name)

  # starts data frame is binary, used to track first observation for each reactor, considered the start
  starts <- dat[, c(id.name, time.name)]
  starts$start <- FALSE
  for(i in unique(dat[, id.name])) {
    starts[starts[, id.name]==i, 'start'][1] <- TRUE
  }

  # Calculate biogas production
  if(any(dat[, 'mass.tot'] < 0)) {
    dat[whichones <- which(dat$mass.tot < 0), 'mass.tot'] <- NA
    stop('Mass *gain* calculated for one or more observations. See ', 
         paste('id.name column:', dat[whichones, id.name], ' and time.name column:', 
               dat[whichones - 1, time.name], 'to', dat[whichones, time.name], sep = ' ', 
               collapse = ', '), 
         ' in dat data frame. ')
  }

  if (cmethod == 'removed') {
    # Method 1
    dat[, c('vBg', 'vCH4')] <- mass2vol(mass = dat[, 'mass.tot'], xCH4 = dat[, xCH4.name], xN2 = 0, 
                                         temp = dat[, temp], pres = dat[, pres], temp.std = temp.std, 
                                         pres.std = pres.std, unit.temp = unit.temp, unit.pres = unit.pres, 
                                         value = 'all', std.message = std.message)[, c('vBg', 'vCH4')]
  } else {
    # Method 2
    dat[, c('vBg', 'vCH4')] <- mass2vol(mass = dat[, 'mass.tot'], xCH4 = dat[, xCH4.name], 
                                         xN2 = dat[, xN2.name], xCO2 =  dat[, xCO2.name],
                                         temp = dat[, temp], pres = dat[, pres], temp.std = temp.std, 
                                         pres.std = pres.std, unit.temp = unit.temp, unit.pres = unit.pres, 
                                         value = 'all', std.message = std.message)[, c('vBg', 'vCH4')]

    dat$vhsCH4 <- dat[, xCH4.name] *
                     stdVol(dat[, vol.hs.name], temp = dat[, temp], pres = dat[, pres.resid], rh = 1, 
                            pres.std = pres.std, temp.std = temp.std, unit.temp = unit.temp, 
                            unit.pres = unit.pres, std.message = std.message)
  }

  if(cmethod == 'removed' & !is.null(headspace)[1]) {
    # Apply initial headspace correction only for times 1 and 2 (i.e., start and 1 following time, one mass loss measurement per reactor)
    which1and2 <- sort(c(which(starts$start), which(starts$start) + 1) )
    dat[which1and2, c('vBg', 'vCH4')] <- mass2vol(mass = dat$mass.tot[which1and2], xCH4 = dat[which1and2, xCH4.name], temp = dat[which1and2, temp], pres = dat[which1and2, pres], temp.std = temp.std, pres.std = pres.std, unit.temp = unit.temp, unit.pres = unit.pres, value = 'all', headspace = dat[which1and2, vol.hs.name], headcomp = headcomp, temp.init = temp.init, std.message = FALSE)[, c('vBg', 'vCH4')]
  }

  # Set time zero volumes to zero--necessary because xCH4 is always missing
  dat[dat$mass.tot==0, c('vBg', 'vCH4')] <- 0

  # Calculation of vCH4 by difference when cmethod = 'total' and interval = FALSE
  if(cmethod == 'total') {
    dat[dat$mass.tot==0, c('vhsCH4')] <- 0
  }

  # Cumulative gas production and rates
  dat <- dat[order(dat[, id.name], dat[, time.name]), ]
  # Calculate delta t for rates
  if(class(dat[, time.name])[1] %in% c('numeric', 'integer')) {
    dt <- c(NA, diff(dat[, time.name]))
  } else if(class(dat[, time.name])[1] %in% c('POSIXct', 'POSIXlt')) {
    dt <- c(NA, as.numeric(diff(dat[, time.name]), units = 'days'))
  } else {
    dt <- NA
    warning('time column in dat data frame not recognized, so rates will not be calculated.')
  }
  # Set dt to NA for the first observation for each reactor
  dt[c(TRUE, dat[, id.name][-1] != dat[, id.name][-nrow(dat)])] <- NA

  for(i in unique(dat[, id.name])) {
    dat[dat[, id.name]==i, 'cvBg']<- cumsum(dat[dat[, id.name]==i, 'vBg' ])
    dat[dat[, id.name]==i, 'cvCH4'] <- cumsum(dat[dat[, id.name]==i, 'vCH4'])
  }

  # For method 2, when cmethod = 'total', cvCH4 must be (re)calculated from cvCH4, 
  # because vhsCH4 is added to cvCH4 (correctly)
  # vBg is not affected by cmethod = 'total'
  if(cmethod == 'total') {
    dat$cvCH4 <- dat$cvCH4 + dat$vhsCH4
    for(i in unique(dat[, id.name])) {
      dat[dat[, id.name]==i, 'vCH4'] <- diff(c(0, dat[dat[, id.name]==i, 'cvCH4']))
    }
  }
  
  # Method 1 & 2
  # Calculate rates for all cases 
  for(i in unique(dat[, id.name])) {
    dat[dat[, id.name]==i, 'rvBg'] <- dat[dat[, id.name]==i, 'vBg' ]/dt[dat[, id.name]==i]
    dat[dat[, id.name]==i, 'rvCH4']<- dat[dat[, id.name]==i, 'vCH4' ]/dt[dat[, id.name]==i]
  }
  
  # Drop time 0 or initial times, works even if time column not recognized
  if(!showt0) {
    dat <- dat[!starts$start, ]
  }

  # Sort and return results
  dat <- dat[order(dat[, id.name], dat[, time.name]), ]
  rownames(dat) <- 1:nrow(dat)

  return(dat)
}
