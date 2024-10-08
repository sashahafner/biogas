calcBgGD <- function(
  # Main arguments
  dat,
  temp.vol,                # Temperature for gas volume measurement, numeric or column name
  temp.grav,               # Temperature for grav measurement, numeric or column name
  pres.vol,                # Pressure for gas volume measurement, numeric or column name
  pres.grav,               # Pressure for grav measurement, numeric or column name
  id.name,
  time.name,
  vol.name,                # As-measured biogas volume (not standardized)
  m.pre.name = NULL,       # Name of column with mass before venting
  m.post.name,             # Name of column with mass after venting
  comp.name = 'xCH4',      # Name of xCH4 column *added* to the data frame
  vented.mass = FALSE,     # Which type of mass loss to use in calculations for xCH4 (vented or total) 
  averaging = 'final',  # Interval, cumulative, or final mass loss for calculating xCH4?
  temp.init = NULL,        # For GDcomp(), headspace correction 
  pres.init = NULL,        # For GDcomp(), headspace correction
  headspace = NULL,        # For GDcomp(), headspace correction
  vol.hs.name = NULL,      # For GDcomp()
  headcomp = 'N2',         # For headspace correction
  # Calculation method and other settings
  vmethod = 'vol',         # Method for biogas calculations, vol or grav
  comp.lim = c(0, 1),      # Allowed limits on xCH4
  comp.sub = NA,           # Value substituted in when xCH4 is outside comp.lim. Use 'lim' for comp.lim values (e.g., 0, 1), or 'NA' or NA for NA
  imethod = 'linear',
  extrap = FALSE,
  addt0 = TRUE,
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
  checkArgClassValue(dat, 'data.frame')
  checkArgClassValue(temp.vol, c('integer', 'numeric', 'character', 'NULL'))
  checkArgClassValue(temp.grav, c('integer', 'numeric', 'character', 'NULL'))
  checkArgClassValue(pres.vol, c('integer', 'numeric', 'character', 'NULL'))
  checkArgClassValue(pres.grav, c('integer', 'numeric', 'character', 'NULL'))
  checkArgClassValue(id.name, 'character')
  checkArgClassValue(time.name, 'character')
  checkArgClassValue(vol.name, 'character')  
  checkArgClassValue(m.pre.name, c('character', 'NULL'))
  checkArgClassValue(m.post.name, 'character')
  checkArgClassValue(comp.name, 'character')
  checkArgClassValue(vented.mass, 'logical')
  checkArgClassValue(averaging, 'character', expected.values = c('int', 'fin', 'cum', 'interval', 'final', 'cumulative'))
  checkArgClassValue(temp.init, c('integer', 'numeric', 'NULL'))
  checkArgClassValue(pres.init, c('integer', 'numeric', 'NULL'))
  checkArgClassValue(headspace, c('data.frame', 'integer', 'numeric', 'NULL')) # NTS: check
  checkArgClassValue(vol.hs.name, c('character', 'NULL'))
  checkArgClassValue(headcomp, 'character')
  checkArgClassValue(vmethod, 'character', expected.values = c('vol', 'volume', 'grav', 'gravimetric'))
  checkArgClassValue(comp.lim, c('integer', 'numeric'))
  checkArgClassValue(comp.sub, c('logical', 'character'), expected.values = c(NA, 'lim', 'numeric'))
  # Skip imethod, checked in interp
  checkArgClassValue(addt0, 'logical')
  checkArgClassValue(extrap, 'logical')
  checkArgClassValue(showt0, 'logical')
  checkArgClassValue(dry, 'logical')
  checkArgClassValue(std.message, 'logical')
  checkArgClassValue(check, 'logical')
  checkArgClassValue(temp.std, c('integer', 'numeric'))
  checkArgClassValue(pres.std, c('integer', 'numeric'))
  checkArgClassValue(unit.temp, 'character')
  checkArgClassValue(unit.pres, 'character')                   

  # Additional checks
  if (vented.mass & vmethod == 'vol') {
    warning('You specified that vented mass should be used with volumetric calculations. This does not make much sense.')
  }

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

    dat[, comp.name] <- GDComp(mass = dat[, mass.name], vol = dat[, std.vol.name], temp = dat[, temp.grav], 
                             pres = dat[, pres.grav], unit.temp = unit.temp, unit.pres = unit.pres) 

  } else {

    # Note that headspace correction is only applied for final averaging
    for(i in unique(dat[, id.name])) {
      which.id <- which(dat[, id.name]==i)

      if (!is.null(vol.hs.name)) {
        vol.hs <- dat[which.id, vol.hs.name][1]
      } else {
        vol.hs <- NULL
      }

      dat[which.id, comp.name] <- GDComp(mass = sum(dat[which.id, mass.name]), 
                                         vol = sum(dat[which.id, std.vol.name]), 
                                         temp = dat[which.id, temp.grav][1], 
                                         pres = dat[which.id, pres.grav][1], 
                                         vol.hs = vol.hs,
                                         headcomp = headcomp,
                                         temp.init = temp.init, pres.init = pres.init,
                                         unit.temp = unit.temp, unit.pres = unit.pres)
    } 
  }

  # Replace xCH4 values that are beyond limits in lim
  # NTS: some of these checks can go after argument list
  dat[, paste0(comp.name, '.lim.flag')] <- ''
  if(all(!is.null(comp.lim)) & all(!is.na(comp.lim)) & is.numeric(comp.lim) & length(comp.lim) == 2) {
    comp.lim <- sort(comp.lim)
    if(!is.na(comp.sub) & comp.sub == 'lim') {
      dat[dat[, comp.name] < comp.lim[1], paste0(comp.name, '.lim.flag')] <- 'low'
      dat[dat[, comp.name] > comp.lim[2], paste0(comp.name, '.lim.flag')] <- 'high'
      dat[dat[, comp.name] < comp.lim[1], comp.name] <- comp.lim[1]
      dat[dat[, comp.name] > comp.lim[2], comp.name] <- comp.lim[2]
    } else {
      dat[!is.na(dat[, comp.name]) & dat[, comp.name] < comp.lim[1], paste0(comp.name, '.lim.flag')] <- 'low'
      dat[!is.na(dat[, comp.name]) & dat[, comp.name] > comp.lim[2], paste0(comp.name, '.lim.flag')] <- 'high'
      dat[!is.na(dat[, comp.name]) & dat[, comp.name] < comp.lim[1], comp.name] <- NA
      dat[!is.na(dat[, comp.name]) & dat[, comp.name] > comp.lim[2], comp.name] <- NA
    }
  }

  # Warn if there are any NAs in xCH4
  if (any(is.na(dat[, comp.name]))) {
    warning('Some NA values present in calculated xCH4. May cause problems in calculations. You can set comp.sub argument to \"lim\" instead but check results!')
  }

  # Check for bottles with no xCH4 values
  if (any((w0 <- tapply(dat[, comp.name], dat[, id.name], FUN = function(x) sum(!is.na(x)))) == 0)) {
    stop('\nNo xCH4 values available for bottle(s):\n ', names(w0)[w0 == 0], '\nCheck input data.')
  }

  # Proceed with either vol or grav method
  # NTS: This should ultimately be done in a separate function, also called by cumBg() or cumBgVol()
  # Volumetric
  # Function will work with vol and add columns
  if(vmethod %in% c('vol', 'volume')) {
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

  } else if(vmethod %in% c('grav', 'gravimetric')) {

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
      whichones <- which(dat$mass.tot < 0)
      #dat[whichones <- which(dat$mass.tot < 0), 'mass.tot'] <- NA
      warning('Mass *gain* found for one or more observations.',
              '\nMaximum of ', signif(min(dat$mass.tot), 2), ' from bottle ID ', dat[which.min(dat$mass.tot), id.name],
              '\nSee ', 
              paste('id.name column:', dat[whichones, id.name], ' and time.name column:', signif(dat[whichones - 1, time.name], 3), 'to', signif(dat[whichones, time.name], 3), sep = ' ', collapse = ', '), ' in dat data frame.\n Proceeding with gravimetric calculation anyway, but results should be checked.\n ')
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

    # Sort results
    dat <- dat[order(dat[, id.name], dat[, time.name]), ]
    rownames(dat) <- 1:nrow(dat)

    # start is binary, used to track first observation for each bottle, considered the start
    starts <- dat[, c(id.name, time.name)]
    starts$start <- FALSE
    starts[c(TRUE, starts[-1, id.name] != starts[-nrow(starts), id.name]), 'start'] <- TRUE

    # Drop time 0 or initial times, works even if time column not recognized
    # Seems to rely on addition of time 0 above if missing in input
    if(!showt0) {
      dat <- dat[!starts$start, ]
    }

    return(dat)
  }

}
