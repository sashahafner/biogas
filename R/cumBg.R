cumBg <- function(
  # Main arguments
  dat,
  dat.type = 'vol',
  comp = NULL, # Leave NULL for wide and both combos
  temp = NULL,
  pres = NULL,
  interval = TRUE, # When empty.name is used, there is a mix, and interval is ignored
  data.struct = 'long', # long, wide, longcombo, widecombo
  # Column names for volumetric method
  id.name = 'id',
  time.name = 'time',
  dat.name = dat.type, # Will be used for first dat column for data.struct = 'wide'
  comp.name = 'xCH4',  # Use for first comp col for data.struct = 'wide'
  # Additional arguments for manometric and gravimetric methods
  pres.resid = NULL,
  temp.init = NULL,
  pres.init = NULL,
  rh.resid = NULL,
  rh.resid.init = 1,
  headspace = NULL,
  vol.hs.name = 'vol.hs',
  headcomp = 'N2',
  absolute = TRUE,
  pres.amb = NULL,
  # For GCA method
  mol.f.name = NULL,
  vol.syr = NULL,
  # Calculation method and other settings
  cmethod = 'removed',
  imethod = 'linear',
  extrap = FALSE,
  addt0 = TRUE,
  showt0 = TRUE,
  # Additional argument for volumetric data only 
  dry = FALSE,
  empty.name = NULL, # Column name for binary/logical column for when cum vol was reset to zero
  ##gas = 'CH4',
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
  ##unit.mass = getOption('unit.mass', 'g')
){
  # Check arguments
  checkArgClassValue(dat, 'data.frame')
  checkArgClassValue(dat.type, 'character', expected.values = c('vol', 'mass', 'pres', 'volume', 'pressure', 'gca'), case.sens = FALSE)
  checkArgClassValue(comp, c('data.frame', 'integer', 'numeric', 'NULL'))
  checkArgClassValue(temp, c('integer', 'numeric', 'character', 'NULL'))
  checkArgClassValue(pres, c('integer', 'numeric', 'character', 'NULL'))
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
  checkArgClassValue(dry, 'logical')
  checkArgClassValue(empty.name, c('character', 'NULL'))
  checkArgClassValue(std.message, 'logical')
  checkArgClassValue(check, 'logical')
  checkArgClassValue(absolute, 'logical')
  checkArgClassValue(pres.amb, c('integer', 'numeric', 'NULL'))
  checkArgClassValue(vol.syr, c('integer', 'numeric', 'NULL'))

  # Hard-wire rh for now at least
  if(!dry[1]) {
    rh <- 1
  } else {
    rh <- 0
  }

  # Check column names in argument data frames
  # comp needs id (time) xCH4, time optional
  if(!is.null(comp) && class(comp)[1] == 'data.frame' && data.struct[1] == 'long') {
    if(any(missing.col <- !c(id.name, comp.name) %in% names(comp))){
      stop('Specified column(s) in comp data frame (', deparse(substitute(comp)), ') not found: ', c(id.name, comp.name)[missing.col], '.')
    }
  }

  # dat (volume or mass)
  if(data.struct %in% c('long', 'longcombo')) {
    if(any(!c(id.name, time.name, dat.name) %in% names(dat))){
      missing.col <- !c(id.name, time.name, dat.name) %in% names(dat)
      stop('Specified columns in dat data frame (', deparse(substitute(dat)), ') not found: ', paste(c(id.name, time.name, dat.name)[missing.col], collapse = ', '), '.')
    } 
  } else if(data.struct[1] == 'wide') {
    if(any(!c(time.name, dat.name) %in% names(dat))){
      missing.col <- !c(time.name, dat.name) %in% names(dat)
      stop('Specified columns in dat data frame (', deparse(substitute(dat)), ') not found: ', paste(c(time.name, dat.name)[missing.col], collapse = ', '), '.')
    } 
  }

  # Check for headspace argument if it is needed
  if(is.null(headspace)[1] && cmethod[1] == 'total') stop('cmethod is set to \"total\" but headspace argument is not provided.')

  # Check for necessary arguments
  if(dat.type %in% c('pres', 'pressure')) {
    if(is.null(headspace[1])) stop('dat.type is set to \"pres\" but \"headspace\" is not provided.')
    if(is.null(temp.init[1])) stop('dat.type is set to \"pres\" but \"temp.init\" is not provided.')
    if(is.null(pres.resid[1])) stop('dat.type is set to \"pres\" but \"pres.resid\" is not provided.')
    if(is.null(pres.init[1])) stop('dat.type is set to \"pres\" but \"pres.init\" is not provided.')
  }

  # Check for other input errors
  if(!is.null(id.name[1]) && id.name %in% names(dat)) {
    if(any(is.na(dat[, id.name]))) {
      w <- which(is.na(dat[, id.name]))
      stop('Missing values in id.name column! See rows ', paste(w, collapse = ', '), '.')
    }
  }

  # And more
  if(!is.null(empty.name[1]) && !class(dat[, empty.name])[1] %in% c('logical', 'integer', 'numeric')) {
    stop('The empty.name column must be integer, numeric, or logical.')
  }

  if(!is.null(empty.name[1]) && length(unique(dat[, empty.name])) > 2) {
    stop('The empty.name column must be binary.')
  }

  if(!is.null(empty.name[1]) && !data.struct %in% c('long', 'longcombo')) {
    stop('You can only use mixed interval/cumulative data (empty.name argument) with long or longcombo data structure')
  }

  # Convert date.type to lowercase so it is more flexible for users
  dat.type <- tolower(dat.type)
  if(dat.type[1] == 'volume') dat.type <- 'vol'

  if(!is.null(empty.name[1]) && dat.type[1] != 'vol') {
    stop('You can only use empty.name argument with volumetric data')
  }

  ### Set interval to TRUE (interval) if there is a mix of cumulative and interval volume data (intermittent emptying hanging water columns, eudiometer, etc.)
  ##if(!is.null(empty.name)) {
  ##  interval <- FALSE
  ##}

  # For dat (vol, etc) missing values are OK if they are cumulative (NTS: why OK if cumulative? Interpolated?)
  # Applies to wide data
  # But now wide data excepted totally
  if(!is.null(dat.name[1])) {
    if(any(is.na(dat[, dat.name])) && interval[1] && data.struct[1] != 'wide') {
      w <- which(is.na(dat[, dat.name]))
      stop('Missing values in dat.name column! See rows ', paste(w, collapse = ', '), '.')
    }
  }

  if(!is.null(time.name[1])) {
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
  if(data.struct[1] == 'wide') {

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
    if(!is.numeric(comp) && class(comp)[1] == 'data.frame') {
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
  if(!interval[1]) {
    dat <- dat[!is.na(dat[, dat.name]), ]
  }

  # If there are missing values in a longcombo data frame, switch to long
  # NTS: this is not the most efficient approach, maybe revisit
  if(tolower(data.struct)[1] == 'longcombo' && any(is.na(dat[, comp.name]))) {
    comp <- dat[, c(id.name, time.name, comp.name)]
    dat <- dat[, names(dat) != comp.name]

    data.struct <- 'long'
  }

  # Sort out composition data if using long data.struct
  # Skipped for longcombo with no NAs (xCH4 already included in dat)
  # GCA method has no biogas composition
  if(tolower(data.struct[1]) == 'long' && dat.type != 'gca') {

    mssg.no.time <- mssg.interp <- FALSE
    # First sort so can find first observation for mass data to ignore it
    dat <- dat[order(dat[, id.name], dat[, time.name]), ]
    dat[, comp.name] <- NA

    if(!is.null(comp)[1] && class(comp)[1] == 'data.frame'){

      # Drop NAs from comp--this applies to wide, long, and longcombo data.struct
      comp <- comp[!is.na(comp[, comp.name]), ]

      # Interpolate gas composition to times of volume measurements
      for(i in unique(dat[, id.name])) {
        if(dat.type[1] == 'mass' && nrow(dat[dat[, id.name]==i, ]) < 2) stop('There are < 2 observations for reactor ', i,' but dat.type = \"mass\". 
                                                                     You need at least 2 observations to apply the gravimetric method.')
        dc <- comp[comp[, id.name]==i, ]
        if(nrow(dc)==0) stop('No biogas composition data for reactor ', i,' so can\'t interpolate!') 
        if(nrow(dc)>1) {
          # If there is no time column
          if(!time.name[1] %in% names(comp)) stop('Problem with comp  (', deparse(substitute(comp)), 
                                               '): a time column was not found but there is > 1 observation at least for reactor ',i, '.')
          if(dat.type[1] %in% c('vol', 'volume', 'pres', 'pressure')) {
            mssg.interp <- TRUE
            dat[dat[, id.name]==i, comp.name] <- interp(dc[, time.name], dc[, comp.name], time.out = dat[dat[, id.name]==i, time.name], method = imethod, extrap = extrap)
            # Set first value to zero if there is no biogas production (fixes problem with cmethod = total when there is a t0 observation included)
            dat[dat[, id.name]==i & dat[, dat.name]==0, comp.name][1] <- 0
          } else if (dat.type=='mass') {
            # Then ignore first point, since it isn't used anyway--this is just to avoid warning with interp if extrap = FALSE
            mssg.interp <- TRUE
            dat[dat[, id.name]==i, comp.name][-1] <- interp(dc[, time.name], dc[, comp.name], time.out = dat[dat[, id.name]==i, time.name][-1], method = imethod, extrap = extrap)
          }
        } else { # If only one xCH4 value is available, use it for all dat obs if extrap = TRUE or times match, but warn if times don't match
          if(!time.name[1] %in% names(comp)) {
            # If there is no time column in comp
            mssg.no.time <- TRUE
            dat[dat[, id.name]==i, comp.name] <- dc[, comp.name]
          } else {
            # There is a time column in dc/comp
            for(j in 1:nrow(dat[dat[, id.name]==i, ])) {
              if(j > 1 || dat.type[1] != 'mass') { # This just to avoid warning for first observation for mass data
                if(dc[, time.name] == dat[dat[, id.name]==i, time.name][j]) { 
                  # If times match
                  dat[dat[, id.name]==i, comp.name][j] <- dc[, comp.name]
                } else {
                  if(extrap[1]) {
                    dat[dat[, id.name]==i, comp.name][j] <- dc[, comp.name]
                  } else {
                    dat[dat[, id.name]==i, comp.name][j] <- NA
        	          warning('Not enough ', comp.name, ' data (one observation) to interpolate for reactor ', i,' so results will be missing.\n If you prefer, you can use constant extrapolation by setting extrap = TRUE.')
                  }
                }
              }
            }
          }
        }
      }
    } else if (!is.null(comp)[1] && class(comp)[1] %in% c('numeric', 'integer') && length(comp)==1) {
      # Or if a single value is given, use it
      if (!quiet[1]) message('Only a single value was provided for biogas composition (', comp, '), so applying it to all observations.')
      dat[, comp.name] <- comp
    } else if (dat.type[1] != 'gca') {
      # If no composition data is given, just use NA
      dat[, comp.name] <- NA 
    }
    if(!quiet[1] & mssg.no.time[1]) message('A time column was not found in comp (', deparse(substitute(comp)), '), and a single value was used for each reactor.')
    if(!quiet[1] & mssg.interp[1]) message('Biogas composition is interpolated.')

  } 

  # Add headspace if provided
  if(!is.null(headspace)[1]) {
    if(is.numeric(headspace)) {
     dat[, vol.hs.name] <- headspace
    } else if(is.data.frame(headspace)) {       
      # headspace needs id vol
      if(any(!c(id.name, vol.hs.name) %in% names(headspace))){
        missing.col <- !c(id.name, vol.hs.name) %in% names(headspace)
        stop('Columns with names matching id.name or vol.hs.name are not present in headspace data frame: ', c(id.name, vol.hs.name)[missing.col], '.')
      }
      dat <- merge(dat, headspace[ , c(id.name, vol.hs.name)], by = id.name, suffixes = c('', '.hs'))
    } else stop('headspace actual argument not recognized. What is it?')
  }

  # Add temperature and pressure to dat if single numeric values were provided
  if(!is.null(temp)[1]) {
    if(is.numeric(temp)) {
      dat[, 'temperature'] <- temp
      temp <- 'temperature'
    } 
  }

  if(!is.null(pres)[1]) {
    if(is.numeric(pres)) {
      dat[, 'pressure'] <- pres
      pres <- 'pressure' 
    } 
  }

  # Correct composition data if it seems to be a percentage
  if (dat.type[1] != 'gca') {
    if (any(na.omit(dat[, comp.name] > 1))) {
      dat[, comp.name] <- dat[, comp.name]/100
      warning('Methane concentration was > 1.0 mol/mol for at least one observation, so is assumed to be a percentage, and was corrected by dividing by 100. ',
              'Range of new values: ', min(na.omit(dat[, comp.name])), '-', max(na.omit(dat[, comp.name])))
    }
  }

  # Now that all data are in long structure (NTS: also for widecombo?) sort out mixed interval/cumulative data
  if(!is.null(empty.name[1])) {
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

  # Volumetric
  # Function will work with vol and add columns
  if(dat.type[1] %in% c('vol', 'volume', 'pres', 'pressure')) {
    # vol dat needs id time vol

    # Standardize total gas volumes
    # Note that temperature and pressure units are not converted at all in cumBg (but are in stdVol of course)
    if(dat.type[1] %in% c('vol', 'volume')) {
      if(!quiet[1]) message('Working with volume data, applying volumetric method.')
      if(!standardized[1]) {
        if(!is.null(temp)[1] && !is.null(pres)[1]) {
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

    if(dat.type[1] %in% c('pres', 'pressure')) {
       if(!quiet[1]) message('Working with pressure data, pressure measurements are', if (absolute) ' ABSOLUTE' else ' GAUGE', 
              ' If this is incorrect, change \'absolute\' argument to ', !absolute, '.')

      # Add pres.resid to dat if it isn't already present
      if(is.numeric(pres.resid)[1] || is.integer(pres.resid)[1]) {
        dat$pres.resid <- pres.resid
        pres.resid <- 'pres.resid'
      }

      # If absolute != TRUE, calculate absolute pressure and add to dat
      if(!absolute[1]) {
        if(is.null(pres.amb)[1]) stop('Pressure measurements are GAUGE but pres.amb argument was not provided.')

        dat[, aa <- paste0(dat.name, '.abs')] <- dat[, dat.name] + pres.amb
        dat.name <- aa

        dat[, aa <- paste0(pres.resid, '.abs')] <- dat[, pres.resid] + pres.amb
        pres.resid <- aa

        pres.init <- pres.init + pres.amb
      }

      # Add residual rh (after pressure measurement and venting)
      if(interval[1]) {
        if(is.null(rh.resid)[1]) {
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
      if(interval[1]) {

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

        if(cmethod[1] == 'total') {
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
    if(dat.type[1] %in% c('vol', 'volume')) {

      dat$vCH4 <- dat$vBg*dat[, comp.name]

      if(cmethod[1] == 'total') {
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
    if(addt0[1] && !class(dat[, time.name])[1] %in% c('numeric', 'integer', 'difftime')) addt0 <- FALSE
    if(addt0[1] && !any(dat[, time.name]==0)) {
      t0 <- data.frame(id = unique(dat[, id.name]), tt = 0, check.names = FALSE)
      names(t0) <- c(id.name, time.name)
      t0[, 'vBg'] <- t0[, 'vCH4'] <- 0

      # This messy, but needed for calculating vCH4 by diff when this method is used and interval = FALSE
      if(cmethod[1] == 'total') {
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
    if(!interval[1]) {
      dat$cvBg <- dat$vBg
      dat$cvCH4 <- dat$vCH4
    }
    
    # Calculate cumulative production or interval production (depending on interval argument)
    # And calculate rates
    if(interval[1]) {
      for(i in unique(dat[, id.name])) {
        dat[dat[, id.name]==i, 'cvBg'] <- cumsum(dat[dat[, id.name]==i, 'vBg' ])
        dat[dat[, id.name]==i, 'cvCH4'] <- cumsum(dat[dat[, id.name]==i, 'vCH4'])
      } 
    }

    # For cmethod = 'total', add headspace CH4 to cvCH4
    if(dat.type[1] %in% c('vol', 'volume') && cmethod == 'total') {
      dat$cvCH4 <- dat$cvCH4 + dat$vhsCH4
    }

    # For cumulative results, calculate interval production from cvCH4 (down here because it may have headspace CH4 added if cmethod = total) so cannot be combined with cvCH4 calcs above
    # vBg could be moved up, but that means more code
    if(!interval[1]) {
      for(i in unique(dat[, id.name])) {
        dat[dat[, id.name]==i, 'vBg'] <- diff(c(0, dat[dat[, id.name]==i, 'cvBg' ]))
        dat[dat[, id.name]==i, 'vCH4'] <- diff(c(0, dat[dat[, id.name]==i, 'cvCH4']))
      }
    }

    # When cmethod = 'total', cvCH4 must be (re)calculated from cvCH4, because vhsCH4 is added to cvCH4 (correctly)
    # vBg is not affected by cmethod = 'total'
    if(dat.type[1] %in% c('vol', 'volume') && cmethod == 'total') {
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
    if(!showt0[1]) {
      dat <- dat[dat[, time.name] != 0, ]
    }

    # Sort and return results
    dat <- dat[order(dat[, id.name], dat[, time.name]), ]

    if(is.null(comp)[1] && data.struct != 'longcombo') { # NTS: revisit if data.struct is ever expanded
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

  } else if(dat.type[1] == 'mass') {
    # Gravimetric
    # Work with mass
    if(!quiet[1]) message('Working with mass data (applying gravimetric approach).')

    # Check for pressure and temperature--required, but default is NULL (for use of volumetric method without standardization) so must check here 
    if(is.null(temp)[1]) stop('temp argument missing but is required for gravimetric method.')
    if(is.null(pres)[1]) stop('pres argument missing but is required for gravimetric method.')
    # With longcombo separate comp data frame is not needed, only comp.name is needed in main data frame
    if(data.struct[1] == 'longcombo') {
      if(is.null(comp.name)[1]) stop('comp.name argument missing but is required for gravimetric method.')
    } else {
      if(is.null(comp)[1]) stop('comp argument missing but is required for gravimetric method.')
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
    if(!is.null(headspace)[1]) {
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
    if((data.struct != 'longcombo' && is.null(comp)) || (data.struct == 'longcombo' & is.null(comp.name))) {
      mass <- mass[, !names(mass) %in% c(comp.name, 'vCH4', 'cvCH4', 'rvCH4')]
    }
    rownames(mass) <- 1:nrow(mass)
    
    return(mass)

  } else if (dat.type[1] == 'gca') {
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
    if(addt0[1] && !class(dat[, time.name])[1] %in% c('numeric', 'integer', 'difftime')) addt0 <- FALSE
    if(addt0[1] && !any(dat[, time.name]==0)) {
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
