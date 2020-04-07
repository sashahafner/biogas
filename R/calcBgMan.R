calcBgMan <- function(
  # Main arguments
  dat,
  comp = NULL,                # Composition of biogas measurement
  temp,                       # Temperature for biogas measurement
  interval = TRUE,            # Indicates if pressure measurements are from time interval only 
  data.struct = 'longcombo',  # long, wide, longcombo
  # Column names 
  id.name = 'id',             # Name of column containing reactor identification code
  time.name = 'time',         # Name of time column 
  pres.name = 'pres',          # Name of column containing headspace pressure measurements
  comp.name = NULL,           # Name of xCH4 column in the data frame
  # Additional arguments 
  pres.resid = NULL,          # Headspace pressure after venting
  temp.init = NULL,           # Initial headspace temperature
  pres.init = NULL,           # Initial headspace pressure
  rh.resid = NULL,            # Relative humidity of gas in headspace 
  rh.resid.init = 1,          # Initial relative humidity of gas in headspace
  headspace = NULL,           # Name of data frame containing headspace volume(s)
  vol.hs.name = 'vol.hs',     # Name of column containing headspace volume data
  absolute = TRUE,            # Unit of measured headspace pressure
  pres.amb = NULL,            # Absolute ambient pressure
  # Calculation method and other settings
  cmethod = 'removed',        # Method for calculating cumulative methane production
  imethod = 'linear',         # Method used for interpolation of xCH4
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
  checkArgClassValue(temp, c('integer', 'numeric', 'character'))
  checkArgClassValue(interval, 'logical')
  checkArgClassValue(data.struct, 'character', expected.values = c('long', 'wide', 'longcombo'))
  checkArgClassValue(id.name, 'character')
  checkArgClassValue(time.name, 'character')
  checkArgClassValue(pres.name, 'character')
  checkArgClassValue(comp.name, c('character', 'NULL'))
  checkArgClassValue(headspace, c('data.frame', 'integer', 'numeric'))
  checkArgClassValue(vol.hs.name, 'character')
  checkArgClassValue(absolute, 'logical')
  checkArgClassValue(pres.amb, c('integer', 'numeric', 'NULL'))
  checkArgClassValue(temp.init, c('integer', 'numeric'))
  checkArgClassValue(pres.init, c('integer', 'numeric'))
  checkArgClassValue(pres.resid, c('integer', 'numeric', 'character'))
  checkArgClassValue(rh.resid.init, c('integer', 'numeric', 'NULL'), expected.range = c(0, 1))
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

  # Create logical variable showing whether composition data were included
  have.comp <- TRUE
  if(data.struct == 'longcombo') {
    if(is.null(comp.name)) {
      have.comp <- FALSE
    }
  } else {
    if(is.null(comp.name)) {
      stop('comp.name argument needed if data.struct != "longcombo".')
    }
    if(is.null(comp)) {
      have.comp <- FALSE
    }
  }
  

  # Hard-wire rh for now at least
  rh <- 1 # NTS: would it make sense to make this an argument instead? 
  
  # Check for input errors in reactor identification code column
  if(!is.null(id.name) & id.name %in% names(dat)) {
    if(any(is.na(dat[, id.name]))) {
      w <- which(is.na(dat[, id.name]))
      stop('Missing values in id.name column! See rows ', paste(w, collapse = ', '), '.')
    }
  }
  
  # For pressure dat missing values are OK only if they are cumulative (NAs obs can be dropped with no error in cvBg)
  if(!is.null(pres.name)) {
    if(any(is.na(dat[, pres.name])) & interval & data.struct != 'wide') {
      w <- which(is.na(dat[, pres.name]))
      stop('Missing values in pres.name column! See rows ', paste(w, collapse = ', '), '.')
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

  # Data preparation (structuring and sorting)
  # Returns dat as data.struct = 'longcombo'
  dat <- cumBgDataPrep(dat = dat, dat.type = 'pres', dat.name = pres.name, 
                       comp.name = comp.name, id.name = id.name, time.name = time.name, 
                       data.struct = data.struct, comp = comp, 
                       have.comp = have.comp,
                       interval = interval, imethod = imethod, 
                       headspace = headspace, vol.hs.name = vol.hs.name, 
                       temp = temp, pres = NULL, rh = rh, extrap = extrap, 
                       temp.std = temp.std, pres.std = pres.std, unit.temp = unit.temp,
                       unit.pres = unit.pres, std.message = std.message, check = check)
  
  # Temperature was added to dat if single numeric values were provided
  if(!is.null(temp)) {
    if(is.numeric(temp)) {
      dat[, 'temperature'] <- temp
      temp <- 'temperature'
    } 
  }
  
  # For data.struct = 'wide', data and composition names are fixed, added manually in cumBgDataPrep()
  if(data.struct == 'wide') {
    pres.name <- 'vol'
    if(have.comp) {
      comp.name <- 'xCH4'
    }
  }

  # Manometric calculation methods
  # Function needs id, time, and pres and will add columns
  # Note that temperature and pressure units are not converted at all in cumBg (but are in stdVol of course)
  if(!quiet) message('Pressure measurements are', if (absolute) ' ABSOLUTE.' else ' GAUGE.', 
                      ' If this is incorrect, change \'absolute\' argument to ', !absolute, '.')
      
  # Add pres.resid to dat if it isn't already present
  if(is.numeric(pres.resid) | is.integer(pres.resid)) {
    dat$pres.resid <- pres.resid
    pres.resid <- 'pres.resid'
  }
      
  # If absolute != TRUE, calculate absolute pressure and add to dat
  if(!absolute) {
    if(is.null(pres.amb)) stop('Pressure measurements are GAUGE but pres.amb argument was not provided.')
        
    dat[, aa <- paste0(pres.name, '.abs')] <- dat[, pres.name] + pres.amb
    pres.name <- aa
        
    dat[, aa <- paste0(pres.resid, '.abs')] <- dat[, pres.resid] + pres.amb
    pres.resid <- aa
        
    pres.init <- pres.init + pres.amb
  }
      
  # Add residual rh (after pressure measurement and venting)
  if(interval) {
    if(is.null(rh.resid)) {
      dat$rh.resid <- dat[, pres.resid]/dat[, pres.name]
      dat$rh.resid[dat$rh.resid > 1] <- 1
    } else {
      dat$rh.resid <- rh.resid
    }
  }
      
  # Sort to add *previous* residual pressure, rh, and temperature columns
  dat <- dat[order(dat[, id.name], dat[, time.name]), ]
  
  # Standardize total gas volumes and calculate biogas and methane production     
  # Standardized headspace volume before venting
  vHS <- stdVol(dat[, vol.hs.name], temp = dat[, temp], pres = dat[, pres.name], rh = rh, 
                pres.std = pres.std, temp.std = temp.std, unit.temp = unit.temp, 
                unit.pres = unit.pres, std.message = FALSE, warn = FALSE) 
    
  # Calculate volume of gas in bottle headspace
  if(interval) {
        
    # Add previous residual pressure, residual rh, temperature, and residual xCH4 columns
    for(i in unique(dat[, id.name])) {
      pr <- dat[dat[, id.name]==i, pres.resid]
      tt <- dat[dat[, id.name]==i, temp]
      rhr <- dat[dat[, id.name]==i, 'rh.resid']
      if(have.comp) {
        xr <- dat[dat[, id.name]==i, comp.name]
      }
      dat[dat[, id.name]==i, 'pres.resid.prev'] <- c(pres.init, pr[-length(pr)])
      dat[dat[, id.name]==i, 'rh.resid.prev'] <- c(rh.resid.init, rhr[-length(rhr)])
      dat[dat[, id.name]==i, 'temp.prev'] <- c(temp.init, tt[-length(tt)])
      if(have.comp) {
        dat[dat[, id.name]==i, paste0(comp.name, '.prev')] <- c(0, xr[-length(xr)])
      }
    }
        
    # Standardize residual headspace volume at end of previous interval
    vHSr <- stdVol(dat[, vol.hs.name], temp = dat[, 'temp.prev'], pres = dat$pres.resid.prev, 
                   rh = dat$rh.resid.prev,  pres.std = pres.std, temp.std = temp.std, 
                   unit.temp = unit.temp, unit.pres = unit.pres, std.message = std.message,
                   warn = FALSE)
        
    # Calculate biogas production, vBg
    dat$vBg <- vHS - vHSr
      
    # Calculate methane production, vCH4
    # Method 1  
    if(have.comp) {
      if(cmethod == 'removed') {
        dat$vCH4 <- dat$vBg*dat[, comp.name]
      } else { # Method 2
        dat$vCH4 <- vHS * dat[, comp.name] - vHSr * dat[, paste0(comp.name, '.prev')]
      }
    }
    
  } else {
    # For cumulative data
    # Standardize initial headspace volume
    vHSi <- stdVol(dat[, vol.hs.name], temp = temp.init, pres = pres.init, rh = rh.resid.init,  
                    pres.std = pres.std, temp.std = temp.std, unit.temp = unit.temp, 
                    unit.pres = unit.pres, std.message = std.message, warn = FALSE)
        
        
    # vHSi is original bottle headspace (standardized), assumed to start at first pres.resid 
    # Calculate biogas and methane production. These are actually cumulative volumes, changed below.
    dat$vBg <- vHS - vHSi
    if(have.comp) {
      dat$vCH4 <- dat$vBg * dat[, comp.name]
    }
  }
    
  # Add t0 row if requested
  # Not added if column is not numeric, integer, or difftime (e.g., date/time)
  if(addt0 & !class(dat[, time.name])[1] %in% c('numeric', 'integer', 'difftime')) addt0 <- FALSE
  # Not added if there are already zeroes present!
  if(addt0 & !any(dat[, time.name]==0)) {
    t0 <- data.frame(id = unique(dat[, id.name]), tt = 0, check.names = FALSE)
    names(t0) <- c(id.name, time.name)
    t0[, 'vBg'] <- 0 
    
    # This messy, but needed for calculating vCH4 by diff when this method is used and interval = FALSE
    if(have.comp) {
      t0[, 'vCH4'] <- 0
  
      if(cmethod == 'total') {
        t0[, 'vhsCH4'] <- 0
      }
    }
      
    dat <- rbindf(dat, t0)
  }
    
  # Calculate delta t for rates
  dat <- dat[order(dat[, id.name], dat[, time.name]), ]
  
  if(class(dat[, time.name])[1] %in% c('numeric', 'integer', 'difftime')) {
    dt <- c(NA, diff(dat[, time.name]))
  } else if(class(dat[, time.name])[1] %in% c('POSIXct', 'POSIXlt')) {
    dt <- c(NA, as.numeric(diff(dat[, time.name]), units = 'days'))
    if(!quiet) message('Rates are per *day*.')
  } else {
    dt <- NA
    warning('class of time column in dat data frame not recognized, so rates will not be calculated.')
  }
  
  # Set dt to NA for first observations for each reactor
  dt[c(TRUE, dat[, id.name][-1] != dat[, id.name][-nrow(dat)])] <- NA 
    
  # May already have cumulative production, if so move it to cv*, and calculate v* down below
  if(!interval) {
    dat$cvBg <- dat$vBg
    if(have.comp) {
      dat$cvCH4 <- dat$vCH4
    }
  }
    
  # For interval results, calculate cumulative production
  if(interval) {
    for(i in unique(dat[, id.name])) {
      dat[dat[, id.name]==i, 'cvBg'] <- cumsum(dat[dat[, id.name]==i, 'vBg' ])
      if(have.comp) {
        dat[dat[, id.name]==i, 'cvCH4'] <- cumsum(dat[dat[, id.name]==i, 'vCH4'])
      }
    } 
  }
    
  # For cumulative results, calculate interval production from cvCH4
  if(!interval) {
    for(i in unique(dat[, id.name])) {
      dat[dat[, id.name]==i, 'vBg'] <- diff(c(0, dat[dat[, id.name]==i, 'cvBg' ]))
      if(have.comp) {
        dat[dat[, id.name]==i, 'vCH4'] <- diff(c(0, dat[dat[, id.name]==i, 'cvCH4']))
      }
    }
  }

  # Calculate production rates
  for(i in unique(dat[, id.name])) {
    dat[dat[, id.name]==i, 'rvBg'] <- dat[dat[, id.name]==i, 'vBg' ]/dt[dat[, id.name]==i]
    if(have.comp) {
      dat[dat[, id.name]==i, 'rvCH4']<- dat[dat[, id.name]==i, 'vCH4' ]/dt[dat[, id.name]==i]
    }
  }
    
  # Drop t0 if not requested (whether originally present or added)
  if(!showt0) {
    dat <- dat[dat[, time.name] != 0, ]
  }
    
  # Sort and return results
  dat <- dat[order(dat[, id.name], dat[, time.name]), ]
    
  if(!have.comp) {
    warning('Biogas composition date (\'comp\' argument) not provided so CH4 results will not be returned.')
    #dat <- dat[, ! names(dat) %in% c(comp.name, 'vCH4', 'cvCH4', 'rvCH4')]
  }
    
  if(all(is.na(dt))) {
    dat <- dat[, ! grepl('^rv', names(dat))]
  }
    
  # Drop NAs if they extend to the latest time for a given bottle (based on problem with wide (originally AMPTSII) data, sometimes shorter for some bottles)
  if(any(is.na(dat[, pres.name]))) {
    
    dat2 <- data.frame()
    
    for(i in unique(dat[, id.name])) {
      
      dd <- dat[dat[, id.name] == i, ]
      
      if(is.na(dd[nrow(dd), pres.name])) {
        # All NAs
        i1 <- which(is.na(dd[, pres.name]))
          
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
  
  # Return results  
  return(dat)
    
} 
