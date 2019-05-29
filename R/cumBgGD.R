cumBgGD <- function(
  # Main arguments
  dat,
  temp.vol,                    # Temperature for gas volume measurement, numeric or column name
  temp.grav,                   # Temperature for grav measurement, numeric or column name
  pres.vol,                    # Pressure for gas volume measurement, numeric or column name
  pres.grav,                   # Pressure for grav measurement, numeric or column name
  id.name = 'id',
  time.name = 'time',
  vol.name = 'vol',        # As-measured biogas volume (not standardized)
  m.pre.name,              # Name of column with mass before venting (NTS: should not be required)
  m.post.name,             # Name of column with mass after venting
  comp.name = 'xCH4',      # Name of xCH4 column *added* to the data frame
  vented.mass = TRUE,      # Which type of mass loss to use in calculations for xCH4 (vented or total) 
  averaging = 'interval',  # Interval, cumulative, or final mass loss for calculating xCH4?
  temp.init = NULL,        # NTS: needs work or deletion. Used for grav calcs
  headspace = NULL,        # 
  vol.hs.name = 'vol.hs',  
  headcomp = 'N2',         # For gravimetric calculations
  # Calculation method and other settings
  vmethod = 'vol',         # Method for biogas calculations, vol or grav or both. NTS: Return warning if vol and venting mass loss is used (or leakage is present)
  comp.lim = c(0, 1),      # Allowed limits on xCH4
  comp.sub = NA,           # Value substituted in when xCH4 is outside comp.lim. Use 'lim' for comp.lim values (e.g., 0, 1), or 'NA' or NA for NA
  imethod = 'linear',
  addt0 = TRUE,
  extrap = FALSE,
  showt0 = TRUE,
  dry = FALSE,
  # Warnings and messages
  std.message = TRUE,
  check = TRUE,
  # Units and standard conditions
  temp.std = getOption('temp.std', as.numeric(NA)),
  pres.std = getOption('pres.std', as.numeric(NA)),
  unit.temp = getOption('unit.temp', 'C'),
  unit.pres = getOption('unit.pres', 'atm')
){

  # Check arguments
  # NTS: add


  # Sort out which mass and volume results to use
  averaging <- substr(averaging, 1, 3)

  if(averaging == 'int') {

    std.vol.name <- 'vBg'

    if(vented.mass) {
      mass.name <- 'mass.vent'
    } else {
      mass.name <- 'mass.tot'
    } 

  } else if(averaging == 'fin') {

    std.vol.name <- 'vBg'

    if(vented.mass) {
      mass.name <- 'mass.vent'
    } else {
      mass.name <- 'mass.tot'
    } 

  } else if(averaging == 'cum') {

    std.vol.name <- 'cvBg'

    if(vented.mass) {
      mass.name <- 'cmass.vent'
    } else {
      mass.name <- 'cmass.tot'
    } 

  }

  # Hard-wire rh for now at least
  if(!dry) {
    rh <- 1
  } else {
    rh <- 0
  }

  # NTS: Add checks from cumBg()

  # Create standardized binary variable that indicates when vBg has been standardized
  standardized <- FALSE

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
  if(!is.null(temp.vol)) {
    if(is.numeric(temp.vol)) {
      dat[, 'temp.vol'] <- temp.vol
      temp.vol <- 'temp.vol'
    } 
  }

  if(!is.null(temp.grav)) {
    if(is.numeric(temp.grav)) {
      dat[, 'temp.grav'] <- temp.grav
      temp.grav <- 'temp.grav'
    } 
  }

  if(!is.null(pres.vol)) {
    if(is.numeric(pres.vol)) {
      dat[, 'pres.vol'] <- pres.vol
      pres.vol <- 'pres.vol' 
    } 
  }

  if(!is.null(pres.grav)) {
    if(is.numeric(pres.grav)) {
      dat[, 'pres.grav'] <- pres.grav
      pres.grav <- 'pres.grav' 
    } 
  }

  # Calculate mass loss 
  dat <- massLoss(dat, time.name = time.name, m.pre.name = m.pre.name, m.post.name = m.post.name, id.name = id.name)

  # Standardize measured biogas volume
  # NTS: These are overwritten below. Might improve.
  dat$vBg <- stdVol(dat[, vol.name], temp = dat[, temp.vol], pres = dat[, pres.vol], rh = rh, pres.std = pres.std, 
                    temp.std = temp.std, unit.temp = unit.temp, unit.pres = unit.pres, 
                    std.message = std.message)

  # Calculate cumulative production 
  dat <- dat[order(dat[, id.name], dat[, time.name]), ]
  for(i in unique(dat[, id.name])) {
    dat[dat[, id.name]==i, 'cvBg'] <- cumsum(dat[dat[, id.name]==i, 'vBg' ])
  } 

  # Get biogas composition
  if(averaging != 'fin') {

    dat[, comp.name] <- gdComp(mass = dat[, mass.name], vol = dat[, std.vol.name], temp = dat[, temp.grav], 
                             pres = dat[, pres.grav], unit.temp = unit.temp, unit.pres = unit.pres, fill.value = 0)

  } else {

    for(i in unique(dat[, id.name])) {
      which.id <- which(dat[, id.name]==i)

      dat[which.id, comp.name] <- gdComp(mass = sum(dat[which.id, mass.name]), 
                                         vol = sum(dat[which.id, std.vol.name]), 
                                         temp = dat[which.id, temp.grav], pres = dat[which.id, pres.grav], 
                                         unit.temp = unit.temp, unit.pres = unit.pres, fill.value = 0)
    } 
  }

  # Replace xCH4 values that are beyond limits in lim
  # NTS: some of these checks can go after argument list
  dat[, paste0(comp.name, '.lim.flag')] <- ''
  if(all(!is.null(comp.lim)) & all(!is.na(comp.lim)) & is.numeric(comp.lim) & length(comp.lim) == 2) {
    if(!is.na(comp.sub) & comp.sub != 'NA') {
      comp.lim <- sort(comp.lim)
      dat[dat[, comp.name] < comp.lim[1], comp.name] <- comp.lim[1]
      dat[dat[, comp.name] > comp.lim[2], comp.name] <- comp.lim[2]
    } else {
      dat[!is.na(dat[, comp.name]) & dat[, comp.name] < comp.lim[1], paste0(comp.name, '.lim.flag')] <- 'low'
      dat[!is.na(dat[, comp.name]) & dat[, comp.name] > comp.lim[2], paste0(comp.name, '.lim.flag')] <- 'high'
      dat[!is.na(dat[, comp.name]) & dat[, comp.name] < comp.lim[1], comp.name] <- NA
      dat[!is.na(dat[, comp.name]) & dat[, comp.name] > comp.lim[2], comp.name] <- NA
    }
  }

  # Proceed with either vol or grav method
  # NTS: This should ultimately be done in a separate function, also called by cumBg() or cumBgVol()
  # Volumetric
  # Function will work with vol and add columns
  if(tolower(vmethod) %in% c('vol', 'volume')) {
    # vol dat needs id time vol

    # Interpolate xCH4 if needed
    # Skip first obs, which should be 0 in grav method
    for(i in unique(dat[, id.name])) {
      dc <- dat[dat[, id.name]==i, ]
      dat[dat[, id.name]==i, comp.name] <- interp(dc[, time.name], dc[, comp.name], time.out = dat[dat[, id.name]==i, time.name], method = imethod, extrap = extrap)
    }

 
    # Calculate CH4 production from vBg calculated above
    if (averaging == 'cum') {
      dat$cvCH4 <- dat[, comp.name] * dat$cvBg
    } else {
      dat$vCH4 <- dat[, comp.name] * dat$vBg
    }

    # Add t0 row if requested
    # Not added if there are already zeroes present!
    if(addt0 & !class(dat[, time.name])[1] %in% c('numeric', 'integer', 'difftime')) addt0 <- FALSE
    if(addt0 & !any(dat[, time.name]==0)) {
      t0 <- data.frame(id = unique(dat[, id.name]), tt = 0, check.names = FALSE)
      names(t0) <- c(id.name, time.name)
      t0[, 'vBg'] <- t0[, 'vCH4'] <- 0
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

    # Calculate cumulative or interval production 
    if (averaging == 'cum') {
      for(i in unique(dat[, id.name])) {
        dat[dat[, id.name]==i, 'vCH4'] <- diff(c(0, dat[dat[, id.name]==i, 'cvCH4']))
      }
    } else {
      for(i in unique(dat[, id.name])) {
        dat[dat[, id.name]==i, 'cvBg'] <- cumsum(dat[dat[, id.name]==i, 'vBg' ])
        dat[dat[, id.name]==i, 'cvCH4'] <- cumsum(dat[dat[, id.name]==i, 'vCH4'])
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

    if(all(is.na(dt))) {
      dat <- dat[, ! names(dat) %in% c('rvBg','rvCH4')]
    }

    rownames(dat) <- 1:nrow(dat)

    return(dat)

  } else if(vmethod == 'grav') {

    # Gravimetric
    message('Working with mass data (applying gravimetric approach).')

    # Interpolate xCH4 if needed
    # Skip first obs, which should be 0 in grav method, so sorting essential
    dat <- dat[order(dat[, id.name], dat[, time.name]), ]
    for(i in unique(dat[, id.name])) {
      dc <- dat[dat[, id.name]==i, ]
      dat[dat[, id.name]==i, comp.name][-1] <- interp(dc[, time.name], dc[, comp.name], time.out = dat[dat[, id.name]==i, time.name][-1], method = imethod, extrap = extrap)
    }

    # Calculate biogas production
    if(any(dat[, 'mass.tot'] < 0)) {
      dat[whichones <- which(dat$mass.tot < 0), 'mass.tot'] <- NA
      stop('Mass *gain* calculated for one or more observations. See ', paste('id.name column:', dat[whichones, id.name], ' and time.name column:', dat[whichones - 1, time.name], 'to', dat[whichones, time.name], sep = ' ', collapse = ', '), ' in dat data frame. ')
    }

    # Calculate CH4 production from vBg calculated above
    if (averaging == 'cum') {
      dat[, c('cvBg', 'cvCH4')] <- mass2vol(mass = dat[, 'cmass.tot'], xCH4 = dat[, comp.name], 
                                          temp = dat[, temp.grav], pres = dat[, pres.grav], 
                                          temp.std = temp.std, pres.std = pres.std, 
                                          unit.temp = unit.temp, unit.pres = unit.pres, 
                                          value = 'all', std.message = FALSE)[, c('vBg', 'vCH4')]
    } else {
      dat[, c('vBg', 'vCH4')] <- mass2vol(mass = dat[, 'mass.tot'], xCH4 = dat[, comp.name], 
                                          temp = dat[, temp.grav], pres = dat[, pres.grav], 
                                          temp.std = temp.std, pres.std = pres.std, 
                                          unit.temp = unit.temp, unit.pres = unit.pres, 
                                          value = 'all', std.message = FALSE)[, c('vBg', 'vCH4')]
    }


    #if(!is.null(headspace)) {
    #  # Apply initial headspace correction only for times 1 and 2 (i.e., one mass loss measurement per reactor)
    #  which1and2 <- sort(c(which(starts$start), which(starts$start) + 1) )
    #  dat[which1and2, c('vBg', 'vCH4')] <- mass2vol(mass = dat$mass.tot[which1and2], xCH4 = dat[which1and2, comp.name], temp = dat[which1and2, temp.grav], pres = dat[which1and2, pres.grav], temp.std = temp.std, pres.std = pres.std, unit.temp = unit.temp, unit.pres = unit.pres, value = 'all', headspace = dat[which1and2, vol.hs.name], headcomp = 'N2', temp.init = temp.init, std.message = FALSE)[, c('vBg', 'vCH4')]
    #}

    # Set time zero volumes to zero--necessary because xCH4 is always missing
    if (averaging == 'cum') {
      dat[dat$mass.tot==0, c('cvBg', 'cvCH4')] <- 0
    } else {
      dat[dat$mass.tot==0, c('vBg', 'vCH4')] <- 0
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

    # Calculate cumulative or interval production 
    if (averaging == 'cum') {
      for(i in unique(dat[, id.name])) {
        dat[dat[, id.name]==i, 'vCH4'] <- diff(c(0, dat[dat[, id.name]==i, 'cvCH4']))
        dat[dat[, id.name]==i, 'vBg'] <- diff(c(0, dat[dat[, id.name]==i, 'cvBg']))
      }
    } else {
      for(i in unique(dat[, id.name])) {
        dat[dat[, id.name]==i, 'cvBg'] <- cumsum(dat[dat[, id.name]==i, 'vBg' ])
        dat[dat[, id.name]==i, 'cvCH4'] <- cumsum(dat[dat[, id.name]==i, 'vCH4'])
      } 
    }

    # Calculate rates for all cases 
    # Rates (rates and v may be strange for averaging = 'cum')
    for(i in unique(dat[, id.name])) {
      dat[dat[, id.name]==i, 'rvBg']<- dat[dat[, id.name]==i, 'vBg' ]/dt[dat[, id.name]==i]
      dat[dat[, id.name]==i, 'rvCH4'] <- dat[dat[, id.name]==i, 'vCH4']/dt[dat[, id.name]==i]
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

}
