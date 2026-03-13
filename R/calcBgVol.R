calcBgVol <- function(
  # Main arguments
  dat,
  comp = NULL,                # Composition of gas measurement
  temp = NULL,                # Temperature for biogas volume measurement
  pres = NULL,                # Pressure for biogas volume measurement
  interval = TRUE,            # When empty.name is used, there is a mix, and interval is ignored
  data.struct = 'longcombo',  # Long, wide, longcombo. Only long data structure can be used. Data restructuring is handled by dataPrep() 
  # Column names
  id.name = 'id',             # Name of column containing reactor identification code
  time.name = 'time',         # Name of time column 
  vol.name = 'vol',           # Name of column containing respons variable, as-measured volume (generally not standardized)
  comp.name = NULL,           # Name of xCH4 column in the data frame
  # Additional arguments
  headspace = NULL,           # Required if cmethod = 'total'
  vol.hs.name = 'vol.hs',     # Name of column containing headspace volume data
  # Calculation method and other settings
  cmethod = 'removed',        # Method for calculation of cumulative methane production
  imethod = 'linear',         # Method for interpolation of xCH4
  extrap = FALSE,
  addt0 = TRUE,
  showt0 = TRUE,
  dry = FALSE,
  empty.name = NULL,          # Column name for binary/logical column for when cum vol was reset to zero
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
  checkArgClassValue(interval, 'logical')
  checkArgClassValue(data.struct, 'character', expected.values = c('long', 'longcombo', 'wide'))
  checkArgClassValue(id.name, 'character')
  checkArgClassValue(time.name, 'character')
  checkArgClassValue(vol.name, 'character')
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

  dat <- as.data.frame(dat)
  if (is.data.frame(comp)) comp <- as.data.frame(comp)
  if (is.data.frame(headspace)) headspace <- as.data.frame(headspace)

  # Create logical variable showing whether composition data were included
  have.comp <- TRUE
  if(data.struct == 'longcombo') {
    if(is.null(comp.name)) {
      have.comp <- FALSE
    }
  } else {
    if(is.null(comp)) {
      have.comp <- FALSE
    }
  }
  
  # Hard-wire rh for now at least
  if(!dry) {
    rh <- 1
  } else {
    rh <- 0
  }
  
  # Continue with more complex argument checks
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
  if(!is.null(empty.name) & !inherits(dat[, empty.name], c('logical', 'integer', 'numeric'))) {
    stop('The empty.name column must be integer, numeric, or logical.')
  }
  
  if(!is.null(empty.name) & length(unique(dat[, empty.name])) > 2) {
    stop('The empty.name column must be binary.')
  }
  
  # For volumetric dat missing values are OK if they are cumulative only (NAs obs can be dropped with no error in cvBg)
  if(!is.null(vol.name)) {
    if(any(is.na(dat[, vol.name])) & interval & data.struct != 'wide') {
      w <- which(is.na(dat[, vol.name]))
      stop('Missing values in vol.name column! See rows ', paste(w, collapse = ', '), '.')
    }
  }
  
  if(!is.null(time.name)) {
    if(any(is.na(dat[, time.name]))) {
      w <- which(is.na(dat[, time.name]))
      stop('Missing values in time.name column! See rows ', paste(w, collapse = ', '), '.')
    }
  } 
  
  # Create standardized binary variable that indicates when vBg has been standardized
  standardized <- FALSE 

  # Check for comp.name if data.struct === 'wide' (for adding column)
  if(data.struct == 'wide' & is.null(comp.name)) {
    comp.name <- 'xCH4'
  }
  
  # Data preparation (structuring and sorting)
  # Returns dat as data.struct = 'longcombo'
  dat <- cumBgDataPrep(dat = dat, dat.type = 'vol', dat.name = vol.name,
                       comp.name = comp.name, id.name = id.name,
                       time.name = time.name, data.struct = data.struct, comp = comp,
                       have.comp = have.comp,
                       interval = interval, imethod = imethod, extrap = extrap,
                       headspace = headspace, vol.hs.name = vol.hs.name,
                       check = check)

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
  
  # For data.struct = 'wide', data and composition names are fixed, added manually in cumBgDataPrep()
  if(data.struct == 'wide') {
    vol.name <- 'vol'
    if(have.comp) {
      comp.name <- 'xCH4'
    }
  }
  
  # Mixed interval/cumulative: standardize volumes and convert to interval
  if(!is.null(empty.name)) {
    dat <- dat[order(dat[, id.name], dat[, time.name]), ]
    dat[, empty.name] <- as.logical(dat[, empty.name])
    dat[is.na(dat[, empty.name]), empty.name] <- FALSE
    dat[, paste0(vol.name, '.std')] <- stdVol(dat[, vol.name], temp = dat[, temp],
                                              pres = dat[, pres], rh = rh, pres.std = pres.std,
                                              temp.std = temp.std, unit.temp = unit.temp,
                                              unit.pres = unit.pres, std.message = std.message)
    std.name <- paste0(vol.name, '.std.interval')
    for(i in unique(dat[, id.name])) {
      emptyvols <- dat[dat[, id.name]==i, empty.name] * dat[dat[, id.name]==i, paste0(vol.name, '.std')]
      ccvv <- c(0, cumsum(emptyvols)[- length(emptyvols)]) + dat[dat[, id.name]==i, paste0(vol.name, '.std')]
      dat[dat[, id.name]==i, std.name] <- diff(c(0, ccvv))
    }
    vol.name <- std.name
    standardized <- TRUE
    interval <- TRUE
  }
  
  # Volumetric method 1
  # Standardize total gas volumes
  # Note that temperature and pressure units are not converted at all in cumBgVol (but are in stdVol)
  if(!standardized) {
    if(!is.null(temp) | !is.null(pres)) {
        dat$vBg <- stdVol(dat[, vol.name], temp = dat[, temp], pres = dat[, pres], rh = rh, 
                          pres.std = pres.std, temp.std = temp.std, unit.temp = unit.temp, 
                          unit.pres = unit.pres, std.message = std.message)
    } else {
        dat$vBg <- dat[, vol.name]
        message('Either temperature or pressure is missing (temp and pres arguments) so volumes are NOT standardized.')
    }
  } else {
    dat$vBg <- dat[, vol.name]
  }
  
  # Calculate interval (or cum if interval = FALSE) methane production
  if(have.comp) {
    dat$vCH4 <- dat$vBg*dat[, comp.name]
  
    # Volumetric method 2  
    # For cmethod = 'total', calculate headspace CH4 to add for total below
    if(cmethod=='total') {
      if(!quiet) message('For cmethod = \"total\", headspace temperature is taken as values in ', temp, ' column, pressure as values in ', pres, ' column, and relative humidity as 1.0 (100%).')
      # Note that rh is assumed to be 1 at all times 
      # Also assume vol meas pressure = residual headspace pressure
      dat$vhsCH4 <- dat[, comp.name] *
                      stdVol(dat[, vol.hs.name], temp = dat[, temp], pres = dat[, pres], rh = 1, 
                             pres.std = pres.std, temp.std = temp.std, unit.temp = unit.temp, 
                             unit.pres = unit.pres, std.message = std.message)
    }
      # vhsCH4 is added to cvCH4 below
      # Calculations are up here to avoid t0 issues
  }

  # Add t0 row if requested
  # Not added if column is not numeric, integer, or difftime (e.g., date/time)
  if(addt0 & !inherits(dat[, time.name], c('numeric', 'integer', 'difftime'))) addt0 <- FALSE
  # Not added if there are already zeroes present!
  if(addt0 & !any(dat[, time.name]==0)) {
    t0 <- data.frame(id = unique(dat[, id.name]), tt = 0, check.names = FALSE)
    names(t0) <- c(id.name, time.name)
    t0[, 'vBg'] <- 0 
    
    if(have.comp) {
      t0[, 'vCH4'] <- 0
  
      # Calculation of vCH4 by difference when cmethod = 'total' and interval = FALSE
      if(cmethod == 'total') {
        t0[, 'vhsCH4'] <- 0
      }
    }

    dat <- rbindf(dat, t0)
  }
  
  # Calculate delta t, cumulative/interval volumes, and rates
  dat <- dat[order(dat[, id.name], dat[, time.name]), ]
  dt  <- calcDeltaT(dat, id.name, time.name, quiet)
  dat <- calcCumVol(dat, id.name, interval, have.comp, cmethod)
  dat <- calcRates(dat, dt, have.comp)
  
  # Drop t0 if not requested (whether originally present or added)
  if(!showt0) {
    dat <- dat[dat[, time.name] != 0, ]
  }
  
  # Sort and return results
  dat <- dat[order(dat[, id.name], dat[, time.name]), ]
  
  # Check for completely missing comp data and omit CH4 results in this case
  if(!have.comp) {
    warning('Biogas composition data (\'comp\' or \'comp.name\' arguments) not provided so only biogas and not CH4 results will be returned.')
    #dat <- dat[, ! names(dat) %in% c(comp.name, 'vCH4', 'cvCH4', 'rvCH4')]
  }
  
  if(all(is.na(dt))) {
    dat <- dat[, ! grepl('^rv', names(dat))]
  }
  
  # Drop NAs if they extend to the latest time for a given bottle (based on problem with AMPTSII data, sometimes shorter for some bottles)
  dat <- trimTrailingNAs(dat, id.name, vol.name)
  
  rownames(dat) <- 1:nrow(dat)
  
  # Return results
  return(dat)
  
} 
