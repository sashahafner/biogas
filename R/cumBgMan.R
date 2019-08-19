cumBgMan <- function(
  # Main arguments
  dat,
  dat.type = 'pres',
  comp = NULL,              # Composition of biogas measurement
  temp = NULL,              # Temperature for biogas measurement
  interval = TRUE,
  data.struct = 'long',     # long, wide, longcombo, widecombo
  # Column names for volumetric method
  id.name = 'id',
  time.name = 'time',
  dat.name = dat.type, # Will be used for first dat column for data.struct = 'wide'
  comp.name = 'xCH4',  # Use for first comp col for data.struct = 'wide'
  # Additional arguments 
  pres.resid = NULL,
  temp.init = NULL,
  pres.init = NULL,
  rh.resid = NULL,
  rh.resid.init = 1,
  headspace = NULL,
  vol.hs.name = 'vol.hs',
  absolute = TRUE,
  pres.amb = NULL,
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
  checkArgClassValue(headcomp, 'character')
  checkArgClassValue(temp.init, c('integer', 'numeric', 'NULL'))
  checkArgClassValue(temp.std, c('integer', 'numeric'))
  checkArgClassValue(pres.std, c('integer', 'numeric'))
  checkArgClassValue(pres.resid, c('integer', 'numeric', 'character', 'NULL'))
  checkArgClassValue(pres.init, c('integer', 'numeric', 'NULL'))
  checkArgClassValue(rh.resid.init, c('integer', 'numeric', 'NULL'), expected.range = c(0, 1))
  checkArgClassValue(unit.temp, 'character')
  checkArgClassValue(unit.pres, 'character')
  checkArgClassValue(cmethod, 'character', expected.values = c('removed', 'total'))
  # Skip imethod, checked in interp
  checkArgClassValue(extrap, 'logical')
  checkArgClassValue(addt0, 'logical')
  checkArgClassValue(showt0, 'logical')
  checkArgClassValue(std.message, 'logical')
  checkArgClassValue(check, 'logical')
  checkArgClassValue(absolute, 'logical')
  checkArgClassValue(pres.amb, c('integer', 'numeric', 'NULL'))

  # Hard-wire rh for now at least
  # NTS: This argument should no be necessary, when not working with vol data
  if(!dry) {
    rh <- 1
  } else {
    rh <- 0
  }
  
  
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

  # Add temperature to dat if single numeric values were provided
  if(!is.null(temp)) {
    if(is.numeric(temp)) {
      dat[, 'temperature'] <- temp
      temp <- 'temperature'
    } 
  }
  
  # NTS Q for Nanna: this was not correct was it? Not standardized by default. And did we lose this argument? 
  # NTS: Revisit below.
  #standardized <- TRUE # We need this argument and if TRUE dat should have vBg = vol below 
  standardized <- FALSE
  #interval <- TRUE # NTS Q for Nanna: Where is interval/cumulative sorted out now? Do we need to do it after stdVol()?
  
  # Manometric 
  # Function will work with man and add columns
  if(dat.type %in% c('pres', 'pressure')) {
    
    # Standardize total gas volumes
    # Note that temperature and pressure units are not converted at all in cumBg (but are in stdVol of course)
    if(dat.type %in% c('vol', 'volume')) {
      if(!quiet) message('Working with volume data, applying volumetric method.')
      if(!standardized) {
        if(!is.null(temp) & !is.null(pres)) {
          dat$vBg <- stdVol(dat[, dat.name], temp = dat[, temp], pres = dat[, pres], rh = rh, pres.std = pres.std, 
                            temp.std = temp.std, unit.temp = unit.temp, unit.pres = unit.pres, 
                            std.message = std.message)
        } else {
          dat$vBg <- dat[, dat.name]
          message('Either temperature or presure is missing (temp and pres arguments) so volumes are NOT standardized.')
        }
      } else {
        dat$vBg <- dat[, dat.name]
      }
    } 
    
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
    
    # Calculate interval (or cum if interval = FALSE) gas production
    # For cmethod = 'total', calculate headspace CH4 to add for total below
    if(dat.type %in% c('vol', 'volume')) {
      
      dat$vCH4 <- dat$vBg*dat[, comp.name]
      
      if(cmethod=='total') {
        # NTS: message needs to be fixed due to change in temp to column in dat
        #if(!quiet) message('For cmethod = \"total\", headspace temperature is taken as temp (', temp, unit.temp, '), pressure as \"pres\" (', pres, unit.pres, '), and relative humidity as 1.0 (100%).')
        # NTS: problem with rh assumption here. Will actually be < 1 after gas removal
        # Also assume vol meas pressure pres = residual headspace pressure
        dat$vhsCH4 <- dat[, comp.name]*
          stdVol(dat[, vol.hs.name], temp = dat[, temp], pres = dat[, pres], rh = 1, pres.std = pres.std, 
                 temp.std = temp.std, unit.temp = unit.temp, unit.pres = unit.pres, 
                 std.message = std.message)
      }
      # vhsCH4 is added to cvCH4 below
      # Calculations are up here to avoid t0 issues
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
    # And calculate rates
    if(interval) {
      for(i in unique(dat[, id.name])) {
        dat[dat[, id.name]==i, 'cvBg'] <- cumsum(dat[dat[, id.name]==i, 'vBg' ])
        dat[dat[, id.name]==i, 'cvCH4'] <- cumsum(dat[dat[, id.name]==i, 'vCH4'])
      } 
    }
    
    # For cmethod = 'total', add headspace CH4 to cvCH4
    if(dat.type %in% c('vol', 'volume') & cmethod == 'total') {
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
    if(dat.type %in% c('vol', 'volume') & cmethod == 'total') {
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
    
  } else if(dat.type=='mass') {
    # Gravimetric
    # Work with mass
    if(!quiet) message('Working with mass data (applying gravimetric approach).')
    
    # Check for pressure and temperature--required, but default is NULL (for use of volumetric method without standardization) so must check here 
    if(is.null(temp)) stop('temp argument missing but is required for gravimetric method.')
    if(is.null(pres)) stop('pres argument missing but is required for gravimetric method.')
    # With longcombo separate comp data frame is not needed, only comp.name is needed in main data frame
    if(data.struct == 'longcombo') {
      if(is.null(comp.name)) stop('comp.name argument missing but is required for gravimetric method.')
    } else {
      if(is.null(comp)) stop('comp argument missing but is required for gravimetric method.')
    }
    
    # In this section main data frame is saved to `dat`, and name of response (mass) to `mass.name`
    mass <- dat
    mass.name <- dat.name
    
    # Calculate mass loss
    mass <- mass[order(mass[, id.name], mass[, time.name]), ]
    # starts data frame is binary, used to track first observation for each reactor, considered the start
    starts <- mass[, c(id.name, time.name)]
    starts$start <- FALSE
    for(i in unique(mass[, id.name])) {
      mass[mass[, id.name]==i, 'massloss'] <- c(0, -diff(mass[mass[, id.name]==i, mass.name]))
      mass[mass[, id.name]==i, 'cmassloss'] <- cumsum(mass[mass[, id.name]==i, 'massloss'])
      starts[starts[, id.name]==i, 'start'][1] <- TRUE
    }
    
    # Calculate biogas production
    if(any(mass[, 'massloss'] < 0)) {
      mass[whichones <- which(mass$massloss < 0), 'massloss'] <- NA
      stop('Mass *gain* calculated for one or more observations. See ', paste('id.name column:', mass[whichones, id.name], ' and time.name column:', mass[whichones - 1, time.name], 'to', mass[whichones, time.name], sep = ' ', collapse = ', '), ' in dat data frame. ')
    }
    
    mass[, c('vBg', 'vCH4')] <- mass2vol(mass = mass[, 'massloss'], xCH4 = mass[, comp.name], temp = mass[, temp], pres = mass[, pres], temp.std = temp.std, pres.std = pres.std, unit.temp = unit.temp, unit.pres = unit.pres, value = 'all', std.message = std.message)[, c('vBg', 'vCH4')]
    if(!is.null(headspace)) {
      # Apply initial headspace correction only for times 1 and 2 (i.e., one mass loss measurement per reactor)
      which1and2 <- sort(c(which(starts$start), which(starts$start) + 1) )
      mass[which1and2, c('vBg', 'vCH4')] <- mass2vol(mass = mass$massloss[which1and2], xCH4 = mass[which1and2, comp.name], temp = mass[which1and2, temp], pres = mass[which1and2, pres], temp.std = temp.std, pres.std = pres.std, unit.temp = unit.temp, unit.pres = unit.pres, value = 'all', headspace = mass[which1and2, vol.hs.name], headcomp = 'N2', temp.init = temp.init, std.message = FALSE)[, c('vBg', 'vCH4')]
    }
    # Set time zero volumes to zero--necessary because xCH4 is always missing
    mass[mass$massloss==0, c('vBg', 'vCH4')] <- 0
    
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
      mass[mass[, id.name]==i, 'rvBg']<- mass[mass[, id.name]==i, 'vBg' ]/dt[mass[, id.name]==i]
      mass[mass[, id.name]==i, 'rvCH4'] <- mass[mass[, id.name]==i, 'vCH4']/dt[mass[, id.name]==i]
    }
    
    # Drop time 0 or initial times, works even if time column not recognized
    if(!showt0) {
      mass <- mass[!starts$start, ]
    }
    
    # Sort and return results
    mass <- mass[order(mass[, id.name], mass[, time.name]), ]
    # Drop comp-related columns if comp not provided
    # With longcombo separate comp data frame is not needed, only comp.name is needed in main data frame
    if((data.struct != 'longcombo' & is.null(comp)) | (data.struct == 'longcombo' & is.null(comp.name))) {
      mass <- mass[, !names(mass) %in% c(comp.name, 'vCH4', 'cvCH4', 'rvCH4')]
    }
    rownames(mass) <- 1:nrow(mass)
    
    return(mass)
    
  } else if (dat.type == 'gca') {
    # WIP needs work
    
    mol.name <- dat.name
    
    # If no post-venting value is provided, there was no venting, set to pre-venting value
    dat[is.na(dat[, mol.f.name]), mol.f.name] <- dat[is.na(dat[, mol.f.name]), mol.name]
    
    # CH4 volume in syringe
    # Input data are umol CH4
    vCH4.syr <- dat[, mol.name]/1E6/vol2mol(1, 'CH4', temp = 0, pres = 1, rh = 0, unit.temp = 'C', unit.pres = 'atm', tp.message = FALSE)
    
    # CH4 volume within bottle
    dat$vCH4.bot <- vCH4.syr*dat[, vol.hs.name]/vol.syr
    
    # Residual CH4 after venting
    vCH4.syr <- dat[, mol.f.name]/1E6/vol2mol(1, 'CH4', temp = 0, pres = 1, rh = 0, unit.temp = 'C', unit.pres = 'atm', tp.message = FALSE)
    dat$vCH4.resid <- vCH4.syr*dat[, vol.hs.name]/vol.syr
    dat$vCH4.vent <- dat$vCH4.bot - dat$vCH4.resid
    
    # For column order add vCH4 first (calculated below)
    dat[, 'vCH4'] <- NA
    
    # Sort for calculations
    dat <- dat[order(dat[, id.name], dat[, time.name]), ]
    
    # Calculate vCH4 from cumulative values
    for(i in unique(dat[, id.name])) {
      dat[dat[, id.name]==i, 'cvCH4'] <- dat[dat[, id.name]==i, 'vCH4.bot'] + cumsum(c(0, dat[dat[, id.name]==i, 'vCH4.vent'][-nrow(dat[dat[, id.name]==i, ])]))
      dat[dat[, id.name]==i, 'vCH4'] <- diff(c(0, dat[dat[, id.name]==i, 'cvCH4']))
    }
    
    # Add t0?
    if(addt0 & !class(dat[, time.name])[1] %in% c('numeric', 'integer', 'difftime')) addt0 <- FALSE
    if(addt0 & !any(dat[, time.name]==0)) {
      t0 <- data.frame(id = unique(dat[, id.name]), tt = 0, check.names = FALSE)
      names(t0) <- c(id.name, time.name)
      t0[, 'vCH4.bot'] <- t0[, 'vCH4.resid'] <- t0[, 'vCH4.vent'] <- t0[, 'cvCH4'] <- t0[, 'vCH4'] <- 0
      
      dat <- rbindf(dat, t0)
    }
    
    dat <- dat[order(dat[, id.name], dat[, time.name]), ]
    
    rownames(dat) <- 1:nrow(dat)
    
    return(dat)
    
  } else stop('Problem with dat.type argument.')
}
