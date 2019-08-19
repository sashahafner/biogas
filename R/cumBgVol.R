cumBgVol <- function(
  # Main arguments
  dat,
  data.type = 'vol',
  comp = NULL,              # Composition of gas measurement
  temp = NULL,              # Temperature for biogas volume measurement
  pres = NULL,              # Pressure for biogas volume measurement
  interval = TRUE,          # When empty.name is used, there is a mix, and interval is ignored
  data.struct = 'long',     # Long, wide, longcombo. Only long data structure can be used. Data restructuring is handled by dataPrep() 
  # Column names
  id.name = 'id',
  time.name = 'time',
  dat.name = data.type,     # Name of column containing respons variable (volume measurements)
  comp.name = 'xCH4',       # Name of xCH4 column in the data frame
  # Additional arguments
  headspace = NULL,         # Required if cmethod = 'total'
  vol.hs.name = 'vol.hs',   # Name of column containing headspace volume data
  # Calculation method and other settings
  cmethod = 'removed',      # Method for calculation of cumulative methane production
  imethod = 'linear',       # Method for interpolation of xCH4
  extrap = FALSE,
  addt0 = TRUE,
  showt0 = TRUE,
  dry = FALSE,
  empty.name = NULL,        # Column name for binary/logical column for when cum vol was reset to zero
  # Warnings and messages
  std.message = !quiet,
  check = TRUE,
  # Units and standard conditions
  temp.std = getOption('temp.std', as.numeric(NA)),
  pres.std = getOption('pres.std', as.numeric(NA)),
  unit.temp = getOption('unit.temp', 'C'),
  unit.pres = getOption('unit.pres', 'atm'),
  quiet = FALSE ##,
){
  
  # Check arguments
  checkArgClassValue(dat, 'data.frame')
  checkArgClassValue(comp, c('data.frame', 'integer', 'numeric', 'NULL'))
  checkArgClassValue(temp, c('integer', 'numeric', 'character', 'NULL'))
  checkArgClassValue(pres, c('integer', 'numeric', 'character', 'NULL'))
  checkArgClassValue(interval, 'logical')
  checkArgClassValue(data.struct, 'character', expected.values = c('long', 'longcombo', 'wide'))
  checkArgClassValue(id.name, 'character')
  checkArgClassValue(time.name, 'character')
  checkArgClassValue(dat.name, 'character')
  checkArgClassValue(comp.name, c('character', 'NULL'))
  checkArgClassValue(headspace, c('data.frame', 'integer', 'numeric', 'NULL'))
  checkArgClassValue(vol.hs.name, 'character')
  checkArgClassValue(cmethod, 'character', expected.values = c('removed', 'total'))
  # Skip imethod, checked in interp
  checkArgClassValue(extrap, 'logical')
  checkArgClassValue(addt0, 'logical')
  checkArgClassValue(showt0, 'logical')
  checkArgClassValue(dry, 'logical')
  checkArgClassValue(empty.name, c('character', 'NULL'))
  checkArgClassValue(std.message, 'logical')
  checkArgClassValue(check, 'logical')
  checkArgClassValue(temp.std, c('integer', 'numeric'))
  checkArgClassValue(pres.std, c('integer', 'numeric'))
  checkArgClassValue(unit.temp, 'character')
  checkArgClassValue(unit.pres, 'character') 
  
  # Hard-wire rh for now at least
  if(!dry) {
    rh <- 1
  } else {
    rh <- 0
  }
  
  # Check for headspace argument if it is needed
  if(is.null(headspace) & cmethod=='total') stop('cmethod is set to \"total\" but headspace argument is not provided.')
  
  # Check for input errors in reactor identification code column
  if(!is.null(id.name) & id.name %in% names(dat)) {
    if(any(is.na(dat[, id.name]))) {
      w <- which(is.na(dat[, id.name]))
      stop('Missing values in id.name column! See rows ', paste(w, collapse = ', '), '.')
    }
  }
  
  # Check for mixed interval/cumulative data
  if(!is.null(empty.name) & !class(dat[, empty.name])[1] %in% c('logical', 'integer', 'numeric')) {
    stop('The empty.name column must be integer, numeric, or logical.')
  }
  
  if(!is.null(empty.name) & length(unique(dat[, empty.name])) > 2) {
    stop('The empty.name column must be binary.')
  }
  
  # For volumetric dat missing values are OK if they are cumulative only (NTS: why OK if cumulative? Interpolated?)
  if(!is.null(dat.name)) {
    if(any(is.na(dat[, dat.name])) & interval) {
      w <- which(is.na(dat[, dat.name]))
      stop('Missing values in dat.name column! See rows ', paste(w, collapse = ', '), '.')
    }
  }
  
  if(!is.null(time.name)) {
    if(any(is.na(dat[, time.name]))) {
      w <- which(is.na(dat[, time.name]))
      stop('Missing values in time.name column! See rows ', paste(w, collapse = ', '), '.')
    }
  }

  # NTS: Add other checks here (e.g., missing values elsewhere)
  
  # Data preparation (structuring and sorting)
  # Returns dat as data.struct = 'long'
  ## Call cumBgDataPrep function
  dat <- cumBgDataPrep(dat = dat, dat.name = dat.name, comp.name = comp.name, id.name = id.name, 
                       time.name = time.name, data.struct = data.struct, comp = comp, 
                       interval = interval, imethod = imethod, extrap = extrap, 
                       headspace = headspace, vol.hs.name = vol.hs.name, 
                       temp = temp, pres = pres, empty.name = empty.name, 
                       std.message = std.message)

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
  
  # NTS: This section should be deleted. 
  # And continue below with interval data (interval = TRUE)
  # NTS Q for Nanna: this was not correct was it? Not standardized by default. And did we lose this argument? 
  # NTS: Revisit below.
  #standardized <- TRUE # We need this argument and if TRUE dat should have vBg = vol below 
  standardized <- FALSE
  #interval <- TRUE # NTS Q for Nanna: Where is interval/cumulative sorted out now? Do we need to do it after stdVol()?

  # Volumetric calculation methods 
  # Function will work with vol and add columns
  # vol dat needs id time vol
  
  # Volumetric method 1
  # Standardize total gas volumes
  # Note that temperature and pressure units are not converted at all in cumBgVol (but are in stdVol)
  if(!standardized) {
    if(!is.null(temp) & !is.null(pres)) {
        dat$vBg <- stdVol(dat[, dat.name], temp = dat[, temp], pres = dat[, pres], rh = rh, pres.std = pres.std, 
                          temp.std = temp.std, unit.temp = unit.temp, unit.pres = unit.pres, 
                          std.message = std.message)
    } else {
        dat$vBg <- dat[, dat.name]
        message('Either temperature or presure is missing (temp and pres arguments) so volumes are NOT standardized.')
    #} else {
    # dat$vBg <- dat[, dat.name]   # Is this necessary? Same code as just above
    }
  } 
  
  # Calculate interval (or cum if interval = FALSE) methane production
  dat$vCH4 <- dat$vBg*dat[, comp.name]
  
  # Volumetric method 2  
  # For cmethod = 'total', calculate headspace CH4 to add for total below
  if(cmethod=='total') {
    # NTS: message needs to be fixed due to change in temp to column in dat
    #if(!quiet) message('For cmethod = \"total\", headspace temperature is taken as temp (', temp, unit.temp, '), pressure as \"pres\" (', pres, unit.pres, '), and relative humidity as 1.0 (100%).')
    # NTS: problem with rh assumption here. Will actually be < 1 after gas removal
    # Also assume vol meas pressure = residual headspace pressure
    dat$vhsCH4 <- dat[, comp.name]*
      stdVol(dat[, vol.hs.name], temp = dat[, temp], pres = dat[, pres], rh = 1, pres.std = pres.std, 
             temp.std = temp.std, unit.temp = unit.temp, unit.pres = unit.pres, 
             std.message = std.message)
  }
    # vhsCH4 is added to cvCH4 below
    # Calculations are up here to avoid t0 issues
  
  # Add t0 row if requested
  # Not added if there are already zeroes present!
  if(addt0 & !class(dat[, time.name])[1] %in% c('numeric', 'integer', 'difftime')) addt0 <- FALSE
  if(addt0 & !any(dat[, time.name]==0)) {
    t0 <- data.frame(id = unique(dat[, id.name]), tt = 0, check.names = FALSE)
    names(t0) <- c(id.name, time.name)
    t0[, 'vBg'] <- t0[, 'vCH4'] <- 0
  
  # Calculation of vCH4 by diff when cmethod = 'total' and interval = FALSE
    if(cmethod == 'total') {
    t0[, 'vhsCH4'] <- 0
    }
    dat <- rbindf(dat, t0)
  }
  
  # Calculate delta t for rates
  dat <- dat[order(dat[, id.name], dat[, time.name]), ]
  if(class(dat[, time.name])[1] %in% c('numeric', 'integer', 'difftime')) {
    dt <- c(NA, diff(dat[, time.name]))
  } else if(class(dat[, time.name])[1] %in% c('POSIXct', 'POSIXlt')) {
    dt <- c(NA, as.numeric(diff(dat[, time.name]), units = 'days'))
  } else {
    dt <- NA
    warning('class of time column in dat data frame not recognized, so rates will not be calculated.')
  }
  
  # Set dt to NA for first observations for each reactor
  dt[c(TRUE, dat[, id.name][-1] != dat[, id.name][-nrow(dat)])] <- NA 
  
  # May already have cumulative production, if so move it to cvCH4, and calculate vCH4 down below
  if(!interval) {
    dat$cvBg <- dat$vBg
    dat$cvCH4 <- dat$vCH4
  }
  
  # Calculate cumulative production or interval production (depending on interval argument)
  # Method 1
  if(interval && cmethod != 'total') {
    for(i in unique(dat[, id.name])) {
      dat[dat[, id.name]==i, 'cvBg'] <- cumsum(dat[dat[, id.name]==i, 'vBg' ])
      dat[dat[, id.name]==i, 'cvCH4'] <- cumsum(dat[dat[, id.name]==i, 'vCH4'])
    } 
  } else {
    for(i in unique(dat[, id.name])) {
      dat[dat[, id.name]==i, 'vBg'] <- diff(c(0, dat[dat[, id.name]==i, 'cvBg' ]))
      dat[dat[, id.name]==i, 'vCH4'] <- diff(c(0, dat[dat[, id.name]==i, 'cvCH4']))
    }
  }
  
  # Method 2
  # For method 2, cmethod = 'total', add headspace CH4 to cvCH4
  if(cmethod == 'total') {
    dat$cvCH4 <- dat$cvCH4 + dat$vhsCH4
  }
  # For method 2, when cmethod = 'total', cvCH4 must be (re)calculated from cvCH4, because vhsCH4 is added to cvCH4 (correctly)
  # vBg is not affected by cmethod = 'total'
  if(cmethod == 'total') {
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
  
  # Drop t0 if not requested (whether originally present or added)
  if(!showt0) {
    dat <- dat[dat[, time.name] != 0, ]
  }
  
  # Sort and return results
  dat <- dat[order(dat[, id.name], dat[, time.name]), ]
  
  if(is.null(comp.name)) {
    warning('Biogas composition date (\'comp\' and \'comp.name\' arguments) not provided so CH4 results will not be returned.')
    dat <- dat[, ! names(dat) %in% c(comp.name, 'vCH4', 'cvCH4', 'rvCH4')]
  }
  
  if(all(is.na(dt))) {
    dat <- dat[, ! names(dat) %in% c('rvBg','rvCH4')]
  }
  
  # Drop NAs if they extend to the latest time for a given bottle (based on problem with AMPTSII data, sometimes shorter for some bottles)
  if(any(is.na(dat[, dat.name]))) {
    dat2 <- data.frame()
    for(i in unique(dat[, id.name])) {
      dd <- dat[dat[, id.name] == i, ]
      if(is.na(dd[nrow(dd), dat.name])) {
        # All NAs
        i1 <- which(is.na(dd[, dat.name]))
    
        # Look for consecutive NAs
        i1d <- diff(i1)
    
        # That are uninterupted by a value
        if(any(i1d > 1)) {
          i2 <- max(which(i1d > 1)) + 1 
        } else {
          i2 <- 1
        }
    
        i3 <- i1[i2]
    
        dat2 <- rbind(dat2, dd[-c(i3:nrow(dd)), ])
    
      } else {
    
        dat2 <- rbind(dat2, dd)
    
      }
    }
  
    dat <- dat2
  }
  
  rownames(dat) <- 1:nrow(dat)
  
  return(dat)
  
  } 
