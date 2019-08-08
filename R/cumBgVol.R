cumBgVol <- function(
  # Main arguments
  dat,
  comp = NULL,              # Leave NULL for wide and both combos
  temp = NULL,              # Temperature for biogas volume measurement
  pres = NULL,              # Pressure for gas volume measurement
  interval = TRUE,          # When empty.name is used, there is a mix, and interval is ignored
  data.struct = 'long',     # long, wide, longcombo, widecombo
  id.name = 'id',
  time.name = 'time',
  dat.name = 'vol', # Will be used for first dat column for data.struct = 'wide'
  comp.name = 'xCH4',       # Name of xCH4 column in the data frame. Use for first comp col for data.struct = 'wide'
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
  ##unit.vol = getOption('unit.vol', 'ml'),
){
  
  # Check arguments
  checkArgClassValue(dat, 'data.frame')
  checkArgClassValue(comp, c('data.frame', 'integer', 'numeric', 'NULL'))
  checkArgClassValue(temp, c('integer', 'numeric', 'character', 'NULL'))
  checkArgClassValue(pres, c('integer', 'numeric', 'character', 'NULL'))
  checkArgClassValue(interval, 'logical')
  checkArgClassValue(data.struct, 'character', expected.values = c('long', 'wide', 'longcombo'))
  checkArgClassValue(id.name, 'character')
  checkArgClassValue(time.name, 'character')
  checkArgClassValue(dat.name, 'character', expected.values = 'xCH4')
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
  
  # Hard-wire rh for now at least
  if(!dry) {
    rh <- 1
  } else {
    rh <- 0
  }
  
  # Check column names in argument data frames
  # comp needs id (time) xCH4, time optional
  if(!is.null(comp) && class(comp)[1] == 'data.frame' && data.struct == 'long') {
    if(any(missing.col <- !c(id.name, comp.name) %in% names(comp))){
      stop('Specified column(s) in comp data frame (', deparse(substitute(comp)), ') not found: ', c(id.name, comp.name)[missing.col], '.')
    }
  }
  
  # dat (vol, volume)
  if(data.struct %in% c('long', 'longcombo')) {
    if(any(missing.col <- !c(id.name, time.name, dat.name) %in% names(dat))){
      stop('Specified columns in dat data frame (', deparse(substitute(dat)), ') not found: ', paste(c(id.name, time.name, dat.name)[missing.col], collapse = ', '), '.')
    } 
  } else if(data.struct == 'wide') {
    if(any(missing.col <- !c(time.name, dat.name) %in% names(dat))){
      stop('Specified columns in dat data frame (', deparse(substitute(dat)), ') not found: ', paste(c(time.name, dat.name)[missing.col], collapse = ', '), '.')
    } 
  }
  
  # Check for headspace argument if it is needed
  if(is.null(headspace) & cmethod=='total') stop('cmethod is set to \"total\" but headspace argument is not provided.')
  
  # Check for other input errors
  if(!is.null(id.name) & id.name %in% names(dat)) {
    if(any(is.na(dat[, id.name]))) {
      w <- which(is.na(dat[, id.name]))
      stop('Missing values in id.name column! See rows ', paste(w, collapse = ', '), '.')
    }
  }
  
  # And more
  if(!is.null(empty.name) & !class(dat[, empty.name])[1] %in% c('logical', 'integer', 'numeric')) {
    stop('The empty.name column must be integer, numeric, or logical.')
  }
  
  if(!is.null(empty.name) & length(unique(dat[, empty.name])) > 2) {
    stop('The empty.name column must be binary.')
  }
  
  if(!is.null(empty.name) & !data.struct %in% c('long', 'longcombo')) {
    stop('You can only use mixed interval/cumulative data (empty.name argument) with long or longcombo data structure')
  }
  
  ### Set interval to TRUE (interval) if there is a mix of cumulative and interval volume data (intermittent emptying hanging water columns, eudiometer, etc.)
  ##if(!is.null(empty.name)) {
  ##  interval <- FALSE
  ##}
  
  # For dat (vol) missing values are OK if they are cumulative (NTS: why OK if cumulative? Interpolated?)
  # Applies to wide data
  # But now wide data excepted totally
  if(!is.null(dat.name)) {
    if(any(is.na(dat[, dat.name])) & interval & data.struct != 'wide') {
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
  
  # NTS: add checks for column types (catches problem with data read in incorrectly, e.g., from Excel with)
  #if(!is.null(comp) && class(comp)=='data.frame' && data.struct == 'long' && any(is.na(comp[, comp.name]))) stop('Missing data in comp data frame. See rows ', paste(which(is.na(comp[, comp.name])), collapse = ', '), '.')
  # Drop missing comp rows
  
  # NTS: Add other checks here (e.g., missing values elsewhere)
  
  ### This check has been replaced with a conversion below
  ##if(check) {
  ##  # Composition
  ##  if(is.numeric(comp) | is.integer(comp)) {
  ##    if(any(comp < 0 | comp > 1)) {
  ##      warning('Check biogas composition in ', deparse(substitute(comp)), '. One or more values is outside of range 0.0-1.0.')
  ##    }
  ##  } else {
  ##    if(any(comp[, comp.name] < 0 | comp[, comp.name] > 1)) {
  ##      warning('Check biogas composition in ', deparse(substitute(comp)), '$', comp.name, '. One or more values is outside of range 0.0-1.0.')
  ##    }
  ##  }
  ##}
  
  # Create standardized binary variable that indicates when vBg has been standardized
  standardized <- FALSE
  
  # Rearrange wide data (NTS: what about widecombo?)
  if(data.struct == 'wide') {
    
    which.first.col <- which(names(dat) == dat.name)
    dat.name <- dat.type
    
    # Number of reactors
    nr <- ncol(dat) - which.first.col + 1
    
    # Reactor names taken from column names
    ids <- names(dat)[which.first.col:ncol(dat)]
    
    dat2 <- dat
    dat <- dat[ , 1:which.first.col]
    names(dat)[which.first.col] <- dat.name
    # Note check.names, prevented problem with odd time.name names (OBA issue)
    dat <- data.frame(idxyz = ids[1], dat, check.names = FALSE)
    
    for(i in 2:nr - 1) {
      x <- dat2[ , c(1:(which.first.col - 1), which.first.col + i)]
      names(x)[which.first.col] <- dat.name
      x <- data.frame(idxyz = ids[i + 1], x, check.names = FALSE)
      dat <- rbind(dat, x)
    }
    
    # Drop missing dat values and warn
    if (any(is.na(dat[, dat.name]))) {
      warning('Missing values in dat.name column have been dropped!')
      dat <- dat[!is.na(dat[, dat.name]), ]
    }
    
    # Fix id name
    names(dat)[names(dat) == 'idxyz'] <- id.name
    
    # Now for comp
    if(!is.numeric(comp)) {
      which.first.col <- which(names(comp) == comp.name)
      comp.name <- 'xCH4'
      
      # Number of reactors
      if((ncol(comp) - which.first.col + 1) != nr) stop('Apparent number of reactors in dat and comp do not match. Problem with wide data.struct.')
      
      comp2 <- comp
      comp <- comp[ , 1:which.first.col]
      names(comp)[which.first.col] <- comp.name
      comp <- data.frame(idxyz = ids[1], comp, check.names = FALSE)
      
      for(i in 2:nr - 1) {
        x <- comp2[ , c(1:(which.first.col - 1), which.first.col + i)]
        names(x)[which.first.col] <- comp.name
        x <- data.frame(idxyz = ids[i + 1], x, check.names = FALSE)
        comp <- rbind(comp, x)
      }
      
      # Fix id name
      names(comp)[names(comp) == 'idxyz'] <- id.name
    }
    
    data.struct <- 'long'
  }
  
  # Remove missing values for cumulative data only
  if(!interval) {
    dat <- dat[!is.na(dat[, dat.name]), ]
  }
  
  # If there are missing values in a longcombo data frame, switch to long
  # NTS: this is not the most efficient approach, maybe revisit
  if(data.struct == 'longcombo' && any(is.na(dat[, comp.name]))) {
    comp <- dat[, c(id.name, time.name, comp.name)]
    dat <- dat[, names(dat) != comp.name]
    
    data.struct <- 'long'
  }
  
  # Add headspace if provided
  if(!is.null(headspace)) {
    if(is.numeric(headspace)) {
      dat[, vol.hs.name] <- headspace
    } else if(is.data.frame(headspace)) {       
      # headspace needs id vol
      if(any(missing.col <- !c(id.name, vol.hs.name) %in% names(headspace))){
        stop('Columns with names matching id.name or vol.hs.name are not present in headspace data frame: ', c(id.name, vol.hs.name)[missing.col], '.')
      }
      dat <- merge(dat, headspace[ , c(id.name, vol.hs.name)], by = id.name, suffixes = c('', '.hs'))
    } else stop('headspace actual argument not recognized. What is it?')
  }
  
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
  
  # Now that all data are in long structure (NTS: also for widecombo?) sort out mixed interval/cumulative data
  if(!is.null(empty.name)) {
    # Sort by id and time
    dat <- dat[order(dat[, id.name], dat[, time.name]), ]
    
    # Make empty.name logical
    dat[, empty.name] <- as.logical(dat[, empty.name])
    
    # Set missing values to FALSE
    dat[is.na(dat[, empty.name]), empty.name] <- FALSE
    
    # Standardize biogas volume (needed in order to get interval production, and cannot use cum prod because composition wouldn't work)
    dat[, paste0(dat.name, '.std')] <- stdVol(dat[, dat.name], temp = dat[, temp], pres = dat[, pres], rh = rh, pres.std = pres.std, 
                                              temp.std = temp.std, unit.temp = unit.temp, unit.pres = unit.pres, std.message = std.message)
    
    # Sum final volumes to get cumulative volumes, then interval from them (must have interval data here, because that is what biogas composition is for)
    for(i in unique(dat[, id.name])) {
      emptyvols <- dat[dat[, id.name]==i, empty.name] * dat[dat[, id.name]==i, paste0(dat.name, '.std')]
      ccvv <- c(0, cumsum(emptyvols)[- length(emptyvols)]) + dat[dat[, id.name]==i, paste0(dat.name, '.std')]
      dat[dat[, id.name]==i, dnn <- paste0(dat.name, '.std.interval')] <- diff(c(0, ccvv))
    }
    
    dat.name <- dnn
    
    # And continue below with interval data (interval = TRUE)
    standardized <- TRUE
    interval <- TRUE
  }
  
  # Volumetric method I
  # Function will work with vol and add columns
    # vol dat needs id time vol
    
    # Standardize total gas volumes
    # Note that temperature and pressure units are not converted at all in cumBgVol (but are in stdVol of course)
    if(!standardized) {
      if(!is.null(temp) & !is.null(pres)) {
          dat$vBg <- stdVol(dat[, dat.name], temp = dat[, temp], pres = dat[, pres], rh = rh, pres.std = pres.std, 
                            temp.std = temp.std, unit.temp = unit.temp, unit.pres = unit.pres, 
                            std.message = std.message)
      } else {
          dat$vBg <- dat[, dat.name]
          message('Either temperature or presure is missing (temp and pres arguments) so volumes are NOT standardized.')
      } else {
        dat$vBg <- dat[, dat.name]
      }
    } 
    
    # Calculate interval (or cum if interval = FALSE) methane production
    dat$vCH4 <- dat$vBg*dat[, comp.name]
    
  # Volumetric method II  
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
      
      # This messy, but needed for calculating vCH4 by diff when this method is used and interval = FALSE
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
    # And calculate rates
    if(interval) {
      for(i in unique(dat[, id.name])) {
        dat[dat[, id.name]==i, 'cvBg'] <- cumsum(dat[dat[, id.name]==i, 'vBg' ])
        dat[dat[, id.name]==i, 'cvCH4'] <- cumsum(dat[dat[, id.name]==i, 'vCH4'])
      } 
    }
    
    # For cmethod = 'total', add headspace CH4 to cvCH4
    if(cmethod == 'total') {
      dat$cvCH4 <- dat$cvCH4 + dat$vhsCH4
    }
    
    # For cumulative results, calculate interval production from cvCH4 (down here because it may have headspace CH4 added if cmethod = total) so cannot be combined with cvCH4 calcs above
    # vBg could be moved up, but that means more code
    if(!interval) {
      for(i in unique(dat[, id.name])) {
        dat[dat[, id.name]==i, 'vBg'] <- diff(c(0, dat[dat[, id.name]==i, 'cvBg' ]))
        dat[dat[, id.name]==i, 'vCH4'] <- diff(c(0, dat[dat[, id.name]==i, 'cvCH4']))
      }
    }
    
    # When cmethod = 'total', cvCH4 must be (re)calculated from cvCH4, because vhsCH4 is added to cvCH4 (correctly)
    # vBg is not affected by cmethod = 'total'
    if(cmethod == 'total') {
      for(i in unique(dat[, id.name])) {
        dat[dat[, id.name]==i, 'vCH4'] <- diff(c(0, dat[dat[, id.name]==i, 'cvCH4']))
      }
    }
    
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
    
    if(is.null(comp) & data.struct != 'longcombo') { # NTS: revisit if data.struct is ever expanded
      warning('Biogas composition date (\'comp\' and \'name.comp\' arguments) not provided so CH4 results will not be returned.')
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
    
 