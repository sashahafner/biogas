calcBgGrav <- function(
  # Main arguments
  dat,
  comp = NULL, # Leave NULL for wide and both combos
  temp = NULL,
  pres = NULL,
  data.struct = 'longcombo', # long, wide, longcombo, widecombo
  # Column names for volumetric method
  id.name = 'id',
  time.name = 'time',
  mass.name,            # Will be used for first dat column for data.struct = 'wide'
  xCH4.name = 'xCH4',  # Use for first comp col for data.struct = 'wide'
  xCO2.name = NULL,  
  xN2.name = NULL,  
  # Additional arguments for manometric and gravimetric methods
  temp.init = NULL,
  pres.init = NULL,
  pres.amb = NULL,
  #rh.resid = NULL,
  #rh.resid.init = 1,
  headspace = NULL,
  vol.hs.name = 'vol.hs',
  headcomp = 'N2',
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

  ## Check arguments
  #checkArgClassValue(dat, 'data.frame')
  ##checkArgClassValue(dat.type, 'character', expected.values = c('vol', 'mass', 'pres', 'volume', 'pressure', 'gca'), case.sens = FALSE)
  #checkArgClassValue(comp, c('data.frame', 'integer', 'numeric', 'NULL'))
  #checkArgClassValue(temp, c('integer', 'numeric', 'character', 'NULL'))
  #checkArgClassValue(pres, c('integer', 'numeric', 'character', 'NULL'))
  #checkArgClassValue(interval, 'logical')
  #checkArgClassValue(data.struct, 'character', expected.values = c('long', 'wide', 'longcombo'))
  #checkArgClassValue(id.name, 'character')
  #checkArgClassValue(time.name, 'character')
  #checkArgClassValue(dat.name, 'character')
  #checkArgClassValue(xCH4.name, c('character', 'NULL'))
  #checkArgClassValue(headspace, c('data.frame', 'integer', 'numeric', 'NULL'))
  #checkArgClassValue(vol.hs.name, 'character')
  #checkArgClassValue(headcomp, c('character', 'NULL'))
  #checkArgClassValue(temp.init, c('integer', 'numeric', 'NULL'))
  #checkArgClassValue(temp.std, c('integer', 'numeric'))
  #checkArgClassValue(pres.std, c('integer', 'numeric'))
  #checkArgClassValue(pres.resid, c('integer', 'numeric', 'character', 'NULL'))
  #checkArgClassValue(pres.init, c('integer', 'numeric', 'NULL'))
  #checkArgClassValue(rh.resid.init, c('integer', 'numeric', 'NULL'), expected.range = c(0, 1))
  #checkArgClassValue(unit.temp, 'character')
  #checkArgClassValue(unit.pres, 'character')
  #checkArgClassValue(cmethod, 'character', expected.values = c('removed', 'total'))
  ## Skip imethod, checked in interp
  #checkArgClassValue(extrap, 'logical')
  #checkArgClassValue(addt0, 'logical')
  #checkArgClassValue(showt0, 'logical')
  #checkArgClassValue(dry, 'logical')
  #checkArgClassValue(empty.name, c('character', 'NULL'))
  #checkArgClassValue(std.message, 'logical')
  #checkArgClassValue(check, 'logical')
  #checkArgClassValue(absolute, 'logical')
  #checkArgClassValue(pres.amb, c('integer', 'numeric', 'NULL'))
  #checkArgClassValue(vol.syr, c('integer', 'numeric', 'NULL'))

  # Hard-wire rh for now at least
  rh <- 1

  ## Check column names in argument data frames
  ## comp needs id (time) xCH4, time optional
  #if(!is.null(comp) && class(comp)[1] == 'data.frame' && data.struct[1] == 'long') {
  #  if(any(missing.col <- !c(id.name, xCH4.name) %in% names(comp))){
  #    stop('Specified column(s) in comp data frame (', deparse(substitute(comp)), ') not found: ', c(id.name, xCH4.name)[missing.col], '.')
  #  }
  #}

  ## dat (volume or mass)
  #if(data.struct %in% c('long', 'longcombo')) {
  #  if(any(!c(id.name, time.name, dat.name) %in% names(dat))){
  #    missing.col <- !c(id.name, time.name, dat.name) %in% names(dat)
  #    stop('Specified columns in dat data frame (', deparse(substitute(dat)), ') not found: ', paste(c(id.name, time.name, dat.name)[missing.col], collapse = ', '), '.')
  #  } 
  #} else if(data.struct[1] == 'wide') {
  #  if(any(!c(time.name, dat.name) %in% names(dat))){
  #    missing.col <- !c(time.name, dat.name) %in% names(dat)
  #    stop('Specified columns in dat data frame (', deparse(substitute(dat)), ') not found: ', paste(c(time.name, dat.name)[missing.col], collapse = ', '), '.')
  #  } 
  #}

  ## Check for headspace argument if it is needed
  #if(is.null(headspace)[1] && cmethod[1] == 'total') stop('cmethod is set to \"total\" but headspace argument is not provided.')

  ## Check for necessary arguments
  #if(dat.type %in% c('pres', 'pressure')) {
  #  if(is.null(headspace[1])) stop('dat.type is set to \"pres\" but \"headspace\" is not provided.')
  #  if(is.null(temp.init[1])) stop('dat.type is set to \"pres\" but \"temp.init\" is not provided.')
  #  if(is.null(pres.resid[1])) stop('dat.type is set to \"pres\" but \"pres.resid\" is not provided.')
  #  if(is.null(pres.init[1])) stop('dat.type is set to \"pres\" but \"pres.init\" is not provided.')
  #}

  ## Check for other input errors
  #if(!is.null(id.name[1]) && id.name %in% names(dat)) {
  #  if(any(is.na(dat[, id.name]))) {
  #    w <- which(is.na(dat[, id.name]))
  #    stop('Missing values in id.name column! See rows ', paste(w, collapse = ', '), '.')
  #  }
  #}

  ## And more
  #if(!is.null(empty.name[1]) && !class(dat[, empty.name])[1] %in% c('logical', 'integer', 'numeric')) {
  #  stop('The empty.name column must be integer, numeric, or logical.')
  #}

  #if(!is.null(empty.name[1]) && length(unique(dat[, empty.name])) > 2) {
  #  stop('The empty.name column must be binary.')
  #}

  #if(!is.null(empty.name[1]) && !data.struct %in% c('long', 'longcombo')) {
  #  stop('You can only use mixed interval/cumulative data (empty.name argument) with long or longcombo data structure')
  #}

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

  if (cmethod == 'total' && is.null(pres.amb)) {
    stop('For cmethod = \"total\" pres.amb must be given (column name or numeric value).') 
  }

  # NTS: Need check with xCO2 or xN2 longcombo is only option
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


  # Temperature and pressure were added to dat if single numeric values were provided
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

  if(!is.null(pres.amb)) {
    if(is.numeric(pres.amb)) {
      dat[, 'pres.amb'] <- pres.amb
      pres.amb <- 'pres.amb' 
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

  # In this section main data frame is saved to `dat`, and name of response (mass) to `mass.name`
  mass <- dat

  # Calculate mass loss
  mass <- massLoss(mass, time.name = time.name, m.pre.name = NULL, m.post.name = mass.name, id.name = id.name)

  # starts data frame is binary, used to track first observation for each reactor, considered the start
  starts <- mass[, c(id.name, time.name)]
  starts$start <- FALSE
  for(i in unique(mass[, id.name])) {
    starts[starts[, id.name]==i, 'start'][1] <- TRUE
  }

  # Calculate biogas production
  if(any(mass[, 'mass.tot'] < 0)) {
    mass[whichones <- which(mass$mass.tot < 0), 'mass.tot'] <- NA
    stop('Mass *gain* calculated for one or more observations. See ', paste('id.name column:', mass[whichones, id.name], ' and time.name column:', mass[whichones - 1, time.name], 'to', mass[whichones, time.name], sep = ' ', collapse = ', '), ' in dat data frame. ')
  }

  if (cmethod == 'removed') {
    # Method 1
    mass[, c('vBg', 'vCH4')] <- mass2vol(mass = mass[, 'mass.tot'], xCH4 = mass[, xCH4.name], xN2 = 0, 
                                         temp = mass[, temp], pres = mass[, pres], temp.std = temp.std, 
                                         pres.std = pres.std, unit.temp = unit.temp, unit.pres = unit.pres, 
                                         value = 'all', std.message = std.message)[, c('vBg', 'vCH4')]
  } else {
    # Method 2
    mass[, c('vBg', 'vCH4')] <- mass2vol(mass = mass[, 'mass.tot'], xCH4 = mass[, xCH4.name], 
                                         xN2 = mass[, xN2.name], xCO2 =  mass[, xCO2.name],
                                         temp = mass[, temp], pres = mass[, pres], temp.std = temp.std, 
                                         pres.std = pres.std, unit.temp = unit.temp, unit.pres = unit.pres, 
                                         value = 'all', std.message = std.message)[, c('vBg', 'vCH4')]

    mass$vhsCH4 <- dat[, xCH4.name] *
                     stdVol(dat[, vol.hs.name], temp = dat[, temp], pres = dat[, pres.amb], rh = 1, 
                            pres.std = pres.std, temp.std = temp.std, unit.temp = unit.temp, 
                            unit.pres = unit.pres, std.message = std.message)
  }

  if(cmethod == 'removed' & !is.null(headspace)[1]) {
    # Apply initial headspace correction only for times 1 and 2 (i.e., one mass loss measurement per reactor)
    which1and2 <- sort(c(which(starts$start), which(starts$start) + 1) )
    mass[which1and2, c('vBg', 'vCH4')] <- mass2vol(mass = mass$mass.tot[which1and2], xCH4 = mass[which1and2, xCH4.name], temp = mass[which1and2, temp], pres = mass[which1and2, pres], temp.std = temp.std, pres.std = pres.std, unit.temp = unit.temp, unit.pres = unit.pres, value = 'all', headspace = mass[which1and2, vol.hs.name], headcomp = headcomp, temp.init = temp.init, std.message = FALSE)[, c('vBg', 'vCH4')]
  }

  # Set time zero volumes to zero--necessary because xCH4 is always missing
  mass[mass$mass.tot==0, c('vBg', 'vCH4')] <- 0

  # Calculation of vCH4 by difference when cmethod = 'total' and interval = FALSE
  if(cmethod == 'total') {
    mass[mass$mass.tot==0, c('vhsCH4')] <- 0
  }

  # Cumulative gas production and rates
  mass <- mass[order(mass[, id.name], mass[, time.name]), ]
  # Calculate delta t for rates
  if(class(mass[, time.name])[1] %in% c('numeric', 'integer')) {
    dt <- c(NA, diff(mass[, time.name]))
  } else if(class(mass[, time.name])[1] %in% c('POSIXct', 'POSIXlt')) {
    dt <- c(NA, as.numeric(diff(mass[, time.name]), units = 'days'))
  } else {
    dt <- NA
    warning('time column in mass data frame not recognized, so rates will not be calculated.')
  }
  # Set dt to NA for the first observation for each reactor
  dt[c(TRUE, mass[, id.name][-1] != mass[, id.name][-nrow(mass)])] <- NA

  for(i in unique(mass[, id.name])) {
    mass[mass[, id.name]==i, 'cvBg']<- cumsum(mass[mass[, id.name]==i, 'vBg' ])
    mass[mass[, id.name]==i, 'cvCH4'] <- cumsum(mass[mass[, id.name]==i, 'vCH4'])
  }

  # For method 2, when cmethod = 'total', cvCH4 must be (re)calculated from cvCH4, 
  # because vhsCH4 is added to cvCH4 (correctly)
  # vBg is not affected by cmethod = 'total'
  if(cmethod == 'total') {
    mass$cvCH4 <- mass$cvCH4 + mass$vhsCH4
    for(i in unique(mass[, id.name])) {
      mass[mass[, id.name]==i, 'vCH4'] <- diff(c(0, mass[mass[, id.name]==i, 'cvCH4']))
    }
  }
  
  # Method 1 & 2
  # Calculate rates for all cases 
  for(i in unique(mass[, id.name])) {
    mass[mass[, id.name]==i, 'rvBg'] <- mass[mass[, id.name]==i, 'vBg' ]/dt[mass[, id.name]==i]
    mass[mass[, id.name]==i, 'rvCH4']<- mass[mass[, id.name]==i, 'vCH4' ]/dt[mass[, id.name]==i]
  }
  
  # Drop time 0 or initial times, works even if time column not recognized
  if(!showt0) {
    mass <- mass[!starts$start, ]
  }

  # Sort and return results
  mass <- mass[order(mass[, id.name], mass[, time.name]), ]
  rownames(mass) <- 1:nrow(mass)

  return(mass)
}
