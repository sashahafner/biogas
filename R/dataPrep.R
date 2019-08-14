dataPrep <- function(
  # Main arguments
  dat,
  dat.type = 'vol',
  comp = NULL, # Leave NULL for wide and both combos
  temp = NULL,
  pres = NULL,
  interval = TRUE, # When empty.name is used, there is a mix, and interval is ignored
  data.struct = 'long', # long, wide, longcombo
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
  quiet = FALSE
){

  # Create standardized binary variable that indicates when vBg has been standardized
  standardized <- FALSE
  
  # Rearrange wide data 
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
  if(data.struct == 'longcombo') {
    comp <- dat[, c(id.name, time.name, comp.name)]
    dat <- dat[, names(dat) != comp.name]
    
    data.struct <- 'long'
  }
  
  # Sort out composition data
  # GCA method has no biogas composition
  if(dat.type != 'gca') {
    
    mssg.no.time <- mssg.interp <- FALSE
    # First sort so can find first observation for mass data to ignore it
    dat <- dat[order(dat[, id.name], dat[, time.name]), ]
    dat[, comp.name] <- NA
    
    if(!is.null(comp) && class(comp)[1] == 'data.frame'){
      
      # Drop NAs from comp--this applies to wide, long, and longcombo data.struct
      comp <- comp[!is.na(comp[, comp.name]), ]
      
      # Interpolate gas composition to times of volume measurements
      for(i in unique(dat[, id.name])) {
        if(dat.type=='mass' & nrow(dat[dat[, id.name]==i, ])<2) stop('There are < 2 observations for reactor ', i,' but dat.type = \"mass\". 
                                                                     You need at least 2 observations to apply the gravimetric method.')
        dc <- comp[comp[, id.name]==i, ]
        if(nrow(dc)==0) stop('No biogas composition data for reactor ', i,' so can\'t interpolate!') 
        if(nrow(dc)>1) {
          # If there is no time column
          if(!time.name %in% names(comp)) stop('Problem with comp  (', deparse(substitute(comp)), 
                                               '): a time column was not found but there is > 1 observation at least for reactor ',i, '.')
          if(dat.type %in% c('vol', 'volume', 'pres', 'pressure')) {
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
          if(!time.name %in% names(comp)) {
            # If there is no time column in comp
            mssg.no.time <- TRUE
            dat[dat[, id.name]==i, comp.name] <- dc[, comp.name]
          } else {
            # There is a time column in dc/comp
            for(j in 1:nrow(dat[dat[, id.name]==i, ])) {
              if(j > 1 | dat.type!='mass') { # This just to avoid warning for first observation for mass data
                if(dc[, time.name]==dat[dat[, id.name]==i, time.name][j]) { 
                  # If times match
                  dat[dat[, id.name]==i, comp.name][j] <- dc[, comp.name]
                } else {
                  if(extrap) {
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
    } else if (!is.null(comp) && class(comp)[1] %in% c('numeric', 'integer') && length(comp)==1) {
      # Or if a single value is given, use it
      if (!quiet) message('Only a single value was provided for biogas composition (', comp, '), so applying it to all observations.')
      dat[, comp.name] <- comp
    } else if (dat.type != 'gca') {
      # If no composition data is given, just use NA
      dat[, comp.name] <- NA 
    }
    if(!quiet & mssg.no.time) message('A time column was not found in comp (', deparse(substitute(comp)), '), and a single value was used for each reactor.')
    if(!quiet & mssg.interp) message('Biogas composition is interpolated.')
    
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
  
  # Correct composition data if it seems to be a percentage
  if (dat.type != 'gca') {
    if (any(na.omit(dat[, comp.name] > 1))) {
      dat[, comp.name] <- dat[, comp.name]/100
      warning('Methane concentration was > 1.0 mol/mol for at least one observation, so is assumed to be a percentage, and was corrected by dividing by 100. ',
              'Range of new values: ', min(na.omit(dat[, comp.name])), '-', max(na.omit(dat[, comp.name])))
    }
  }
  
  # Now that all data are in long structure sort out mixed interval/cumulative data
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
}