cumBgMan <- function(
  # Main arguments
  dat,
  dat.type = 'pres',
  comp = NULL,              # Composition of biogas measurement
  temp = NULL,              # Temperature for biogas measurement
  interval = TRUE,
  data.struct = 'long',     # long, wide, longcombo, widecombo
  # Column names 
  id.name = 'id',           # Name of column containing reactor identification code
  time.name = 'time',       # Name of time column 
  dat.name = data.type,     # Name of column containing respons variable (pressure measurements)
  comp.name = 'xCH4',       # Name of xCH4 column in the data frame
  # Additional arguments 
  temp.init = NULL,         # Initial headspace temperature
  pres.init = NULL,         # Initial headspace pressure
  pres.resid = NULL,        # Headspace pressure after venting
  rh.resid = NULL,          # Relative humidity of gas in headspace 
  rh.resid.init = 1,        # Initial relative humidity of gas in headspace
  headspace = NULL,
  vol.hs.name = 'vol.hs',   # Name of column containing headspace volume data
  absolute = TRUE,          # Headspace pressure
  pres.amb = NULL,          # Absolute ambient pressure
  # Calculation method and other settings
  cmethod = 'removed',      # Method for calculating cumulative methane production
  imethod = 'linear',       # Method used for interpolation of xCH4
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
  checkArgClassValue(dat.type, 'character', expected.values = c('pres', 'pressure'), case.sens = FALSE)
  checkArgClassValue(comp, c('data.frame', 'integer', 'numeric', 'NULL'))
  checkArgClassValue(temp, c('integer', 'numeric', 'character', 'NULL'))
  checkArgClassValue(interval, 'logical')
  checkArgClassValue(data.struct, 'character', expected.values = c('long', 'wide', 'longcombo'))
  checkArgClassValue(id.name, 'character')
  checkArgClassValue(time.name, 'character')
  checkArgClassValue(dat.name, 'character')
  checkArgClassValue(comp.name, c('character', 'NULL'))
  checkArgClassValue(headspace, c('data.frame', 'integer', 'numeric', 'NULL'))
  checkArgClassValue(vol.hs.name, 'character')
  checkArgClassValue(absolute, 'logical')
  checkArgClassValue(pres.amb, c('integer', 'numeric', 'NULL'))
  checkArgClassValue(temp.init, c('integer', 'numeric', 'NULL'))
  checkArgClassValue(pres.init, c('integer', 'numeric', 'NULL'))
  checkArgClassValue(pres.resid, c('integer', 'numeric', 'character', 'NULL'))
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

  # Hard-wire rh for now at least
    rh <- 1
  
  # Check for headspace argument if it is needed
  if(is.null(headspace) & cmethod=='total') stop('cmethod is set to \"total\" but headspace argument is not provided.')
  
  # Check for necessary arguments
  if(dat.type %in% c('pres', 'pressure')) {
    if(is.null(headspace)) stop('dat.type is set to \"pres\" but \"headspace\" is not provided.')
    if(is.null(temp.init)) stop('dat.type is set to \"pres\" but \"temp.init\" is not provided.')
    if(is.null(pres.resid)) stop('dat.type is set to \"pres\" but \"pres.resid\" is not provided.')
    if(is.null(pres.init)) stop('dat.type is set to \"pres\" but \"pres.init\" is not provided.')
  }
  
  # Check for other input errors
  if(!is.null(id.name) & id.name %in% names(dat)) {
    if(any(is.na(dat[, id.name]))) {
      w <- which(is.na(dat[, id.name]))
      stop('Missing values in id.name column! See rows ', paste(w, collapse = ', '), '.')
    }
  }
  
  # For pressure dat missing values are OK if they are cumulative (NTS: why OK if cumulative? Interpolated?)
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
  
  # NTS: Add other checks here (e.g., missing values elsewhere)
  
  # Create standardized binary variable that indicates when vBg has been standardized
  standardized <- FALSE

  # Data preparation (structuring and sorting)
  # Returns dat as data.struct = 'long'
  dat <- cumBgDataPrep(dat = dat, dat.type = 'pres', dat.name = dat.name, 
                       comp.name = comp.name, id.name = id.name, time.name = time.name, 
                       data.struct = data.struct, comp = comp, 
                       interval = interval, imethod = imethod, 
                       headspace = headspace, vol.hs.name = vol.hs.name, 
                       temp = temp, pres = NULL, 
                       extrap = extrap, std.message = std.message)
  
  # Add temperature to dat if single numeric values were provided
  if(!is.null(temp)) {
    if(is.numeric(temp)) {
      dat[, 'temperature'] <- temp
      temp <- 'temperature'
    } 
  }
  
  # Data and composition names are added manually for wide data
  if(data.struct == 'wide') {
    dat.name <- 'vol'
    comp.name <- 'xCH4'
  }
 
  # Manometric calculation methods
  # Function will work with man and add columns
    
  # Standardize total gas volumes
    # Note that temperature and pressure units are not converted at all in cumBg (but are in stdVol of course)
    if(dat.type %in% c('pres', 'pressure')) {
      if(!quiet) message('Working with pressure data, pressure measurements are', if (absolute) ' ABSOLUTE' else ' GAUGE', 
                         ' If this is incorrect, change \'absolute\' argument to ', !absolute, '.')
      
      # Add pres.resid to dat if it isn't already present
      if(is.numeric(pres.resid) | is.integer(pres.resid)) {
        dat$pres.resid <- pres.resid
        pres.resid <- 'pres.resid'
      }
      
      # If absolute != TRUE, calculate absolute pressure and add to dat
      if(!absolute) {
        if(is.null(pres.amb)) stop('Pressure measurements are GAUGE but pres.amb argument was not provided.')
        
        dat[, aa <- paste0(dat.name, '.abs')] <- dat[, dat.name] + pres.amb
        dat.name <- aa
        
        dat[, aa <- paste0(pres.resid, '.abs')] <- dat[, pres.resid] + pres.amb
        pres.resid <- aa
        
        pres.init <- pres.init + pres.amb
      }
      
      # Add residual rh (after pressure measurement and venting)
      if(interval) {
        if(is.null(rh.resid)) {
          dat$rh.resid <- dat[, pres.resid]/dat[, dat.name]
          dat$rh.resid[dat$rh.resid > 1] <- 1
        } else {
          dat$rh.resid <- rh.resid
        }
      }
      
      # Sort to add *previous* residual pressure, rh, and temperature columns
      dat <- dat[order(dat[, id.name], dat[, time.name]), ]
      
      # Standardized headspace volume before venting
      vHS <- stdVol(dat[, vol.hs.name], temp = dat[, temp], pres = dat[, dat.name], rh = rh, 
                    pres.std = pres.std, temp.std = temp.std, unit.temp = unit.temp, 
                    unit.pres = unit.pres, std.message = FALSE, warn = FALSE) 
      
      # Finally, calculate volume of gas in bottle headspace
      if(interval) {
        
        # Add previous residual pressure, residual rh, temperature, and residual xCH4 columns
        for(i in unique(dat[, id.name])) {
          pr <- dat[dat[, id.name]==i, pres.resid]
          tt <- dat[dat[, id.name]==i, temp]
          rhr <- dat[dat[, id.name]==i, 'rh.resid']
          xr <- dat[dat[, id.name]==i, comp.name]
          dat[dat[, id.name]==i, 'pres.resid.prev'] <- c(pres.init, pr[-length(pr)])
          dat[dat[, id.name]==i, 'rh.resid.prev'] <- c(rh.resid.init, rhr[-length(rhr)])
          dat[dat[, id.name]==i, 'temp.prev'] <- c(temp.init, tt[-length(tt)])
          dat[dat[, id.name]==i, paste0(comp.name, '.prev')] <- c(0, xr[-length(xr)])
        }
        
        # Residual headspace volume at end of previous interval
        vHSr <- stdVol(dat[, vol.hs.name], temp = dat[, 'temp.prev'], pres = dat$pres.resid.prev, rh = dat$rh.resid.prev,  
                       pres.std = pres.std, temp.std = temp.std, unit.temp = unit.temp, 
                       unit.pres = unit.pres, std.message = std.message)
        
        # vBg is biogas *production*
        dat$vBg <- vHS - vHSr
        
        if(cmethod == 'total') {
          dat$vCH4 <- vHS * dat[, comp.name] - vHSr * dat[, paste0(comp.name, '.prev')]
        } else {
          dat$vCH4 <- dat$vBg*dat[, comp.name]
        }
        
      } else {
        # Initial headspace volume
        vHSi <- stdVol(dat[, vol.hs.name], temp = temp.init, pres = pres.init, rh = rh.resid.init,  
                       pres.std = pres.std, temp.std = temp.std, unit.temp = unit.temp, 
                       unit.pres = unit.pres, std.message = std.message, warn = FALSE)
        
        
        # Second call (subtracted bit) is original bottle headspace (standardized), assumed to start at first pres.resid 
        # These are actually cv cumulative volumes, changed below (set to cv here once this is in a cumBgMan() function)
        dat$vBg <- vHS - vHSi
        dat$vCH4 <- dat$vBg * dat[, comp.name]
      }
    }
    
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
    if(interval) {
      for(i in unique(dat[, id.name])) {
        dat[dat[, id.name]==i, 'cvBg'] <- cumsum(dat[dat[, id.name]==i, 'vBg' ])
        dat[dat[, id.name]==i, 'cvCH4'] <- cumsum(dat[dat[, id.name]==i, 'vCH4'])
      } 
    }
    
    # For cumulative results, calculate interval production from cvCH4 (down here because it may have headspace CH4 added if cmethod = total) so cannot be combined with cvCH4 calcs above
    # vBg could be moved up, but that means more code
    if(!interval) {
      for(i in unique(dat[, id.name])) {
        dat[dat[, id.name]==i, 'vBg'] <- diff(c(0, dat[dat[, id.name]==i, 'cvBg' ]))
        dat[dat[, id.name]==i, 'vCH4'] <- diff(c(0, dat[dat[, id.name]==i, 'cvCH4']))
      }
    }

    # Calculate rates
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
    
    if(is.null(comp)) {
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
    
  } 

