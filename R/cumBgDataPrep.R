cumBgDataPrep <- function(
  # Main arguments
  dat,
  dat.type = 'vol',
  comp = NULL,                # Composition of gas measurement. Leave NULL for wide and longcombo
  temp = NULL,                # Temperature for biogas volume measurement
  pres = NULL,                # Pressure for biogas volume measurement
  rh,
  interval = TRUE,            # When empty.name is used, there is a mix, and interval is ignored
  data.struct = 'longcombo',  # long, wide, longcombo
  # Column names
  id.name = 'id',             # Name of column containing bottle identification code
  time.name = 'time',         # Name of time column 
  dat.name = dat.type,        # Will be used for first dat column for data.struct = 'wide'
  comp.name = 'xCH4',         # Name of column containing xCH4 values. Use for first comp col for data.struct = 'wide'
  have.comp,                  #
  headspace = NULL,           # Required if cmethod = 'total'
  vol.hs.name = 'vol.hs',     # Name of column containing headspace volume data
  # Calculation method and other settings
  imethod = 'linear',         # Method for interpolation of xCH4
  extrap = FALSE,
  # Additional argument for volumetric data only 
  empty.name = NULL,          # Column name for binary/logical column for when cum vol was reset to zero
  # Units
  temp.std,
  pres.std,
  unit.temp,
  unit.pres,
  # Warnings and messages
  std.message = !quiet,
  check = TRUE,
  quiet = FALSE
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
  # Skip imethod, checked in interp
  checkArgClassValue(extrap, 'logical')
  checkArgClassValue(empty.name, c('character', 'NULL'))
  checkArgClassValue(std.message, 'logical')
  checkArgClassValue(check, 'logical')
  
  # Check column names in argument data frames
  # comp needs id (time) xCH4, time optional
  if(!is.null(comp) && any(class(comp) == 'data.frame') && data.struct == 'long') {
    if(any(missing.col <- !c(id.name, comp.name) %in% names(comp))){
      stop('Specified column(s) in comp data frame (', deparse(substitute(comp)), ') not found: ', paste(c(id.name, comp.name)[missing.col], collapse=', '), '.')
    }
  }
  
  # Check missing data in dat data frame (id, time and data)
  if(data.struct %in% c('long', 'longcombo')) {
    if(any(missing.col <- !c(id.name, time.name, dat.name) %in% names(dat))){
      stop('Specified columns in dat data frame (', deparse(substitute(dat)), ') not found: ', paste(c(id.name, time.name, dat.name)[missing.col], collapse = ', '), '.')
    }
  } else if(data.struct == 'wide') {
    if(any(missing.col <- !c(time.name, dat.name) %in% names(dat))){
      stop('Specified columns in dat data frame (', deparse(substitute(dat)), ') not found: ', paste(c(time.name, dat.name)[missing.col], collapse = ', '), '.')
    }
  }
  
  # Rearrange wide data 
  if(data.struct == 'wide') {
    
    which.first.col <- which(names(dat) == dat.name)
    dat.name <- dat.type
    
    # Number of bottles
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
    if(have.comp) {
      if(!is.numeric(comp)) { # If comp is numeric it is added to data frame below
        which.first.col <- which(names(comp) == comp.name)
        comp.name <- 'xCH4'
        
        # Reactor names taken from column names--these could differ between comp and vol
        ids <- names(comp)[which.first.col:ncol(comp)]
        
        # Number of bottles
        if((ncol(comp) - which.first.col + 1) != nr) stop('Apparent number of bottles in dat and comp do not match. Problem with wide data.struct.')
        
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
    }
    data.struct <- 'long'
  }
  
  # Remove missing values for cumulative data only
  if(!interval) {
    dat <- dat[!is.na(dat[, dat.name]), ]
  }
  
  # Rearrange longcombo data
  # If there are missing values in a longcombo data frame, switch to long
  # NTS: this is not the most efficient approach, maybe revisit
  if(have.comp && data.struct == 'longcombo' && any(is.na(dat[, comp.name]))) {
    comp <- dat[, c(id.name, time.name, comp.name)]
    dat <- dat[, names(dat) != comp.name]
    
    data.struct <- 'long'
  }
  
  # Sort out composition data for long data structure
  # GCA method has no biogas composition
  if(have.comp && data.struct == 'long' && dat.type != 'gca') {
    
    mssg.no.time <- mssg.interp <- FALSE
    # First sort to identify first observation for mass data in order to ignore it
    dat <- dat[order(dat[, id.name], dat[, time.name]), ]
    dat[, comp.name] <- NA
    
    if(any(class(comp) == 'data.frame')){
      
      # Drop NAs from comp - this applies to wide, long, and longcombo data.struct
      comp <- comp[!is.na(comp[, comp.name]), ]
      
      # Interpolate gas composition to times of volume measurements
      for(i in unique(dat[, id.name])) {
        if(dat.type=='mass' & nrow(dat[dat[, id.name]==i, ])<2) stop('There are < 2 observations for bottle ', i,' but dat.type = \"mass\". 
                                                                     You need at least 2 observations to apply the gravimetric method.')
        dc <- comp[comp[, id.name]==i, ]
        if(nrow(dc)==0) stop('No biogas composition data for bottle ', i,' so can\'t interpolate!') 
        if(nrow(dc)>1) {
          # If there is no time column
          if(!time.name %in% names(comp)) stop('Problem with comp  (', deparse(substitute(comp)), 
                                               '): a time column was not found but there is > 1 observation at least for bottle ',i, '.')
          if(dat.type %in% c('vol', 'volume', 'pres', 'pressure')) {
            mssg.interp <- TRUE
            dat[dat[, id.name]==i, comp.name] <- interp(dc[, time.name], dc[, comp.name], time.out = dat[dat[, id.name]==i, time.name], method = imethod, extrap = extrap)
            # Set first value to zero if there is no biogas production (fixes problem with cmethod = total when there is a t0 observation included)
            dat[dat[, id.name]==i & dat[, dat.name]==0 & is.na(dat[, comp.name]), comp.name][1] <- 0
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
                    warning('Not enough ', comp.name, ' data (one observation) to interpolate for bottle ', i,' so results will be missing.\n If you prefer, you can use constant extrapolation by setting extrap = TRUE.')
                  }
                }
              }
            }
          }
        }
      }
    } else if (class(comp)[1] %in% c('numeric', 'integer') && length(comp)==1) {
      # Or if a single value is given, use it
      if (!quiet) message('Only a single value was provided for biogas composition (', comp, '), so applying it to all observations.')
      dat[, comp.name] <- comp
    } 

    if(!quiet & mssg.no.time) message('A time column was not found in comp (', deparse(substitute(comp)), '), and a single value was used for each bottle.')
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
  
  # Correct composition data if it seems to be a percentage
  if (have.comp && check) {
    if (any(na.omit(dat[, comp.name] > 1))) {
      dat[, comp.name] <- dat[, comp.name]/100
      warning('Methane concentration was > 1.0 mol/mol for at least one observation, so is assumed to be a percentage, and was corrected by dividing by 100. ',
              'Range of new values: ', min(na.omit(dat[, comp.name])), '-', max(na.omit(dat[, comp.name])))
    }
  }
  
  # Now that all data are in longcombo structure sort out mixed interval/cumulative data followed by standardizing vBg
  # Only applies to mixed interval/cumulative data
  if(!is.null(empty.name)) {
    # Sort by id and time
    dat <- dat[order(dat[, id.name], dat[, time.name]), ]
    
    # Make empty.name logical
    dat[, empty.name] <- as.logical(dat[, empty.name])
    
    # Set missing values to FALSE
    dat[is.na(dat[, empty.name]), empty.name] <- FALSE
    
    # Standardize biogas volume (needed in order to get interval production, and cannot use cum prod because composition wouldn't work)
    dat[, paste0(dat.name, '.std')] <- stdVol(dat[, dat.name], temp = dat[, temp], 
                                              pres = dat[, pres], rh = rh, pres.std = pres.std, 
                                              temp.std = temp.std, unit.temp = unit.temp, 
                                              unit.pres = unit.pres, std.message = std.message)
    
    # Sum final volumes to get cumulative volumes, then interval from them (must have interval data here, because that is what biogas composition is for)
    for(i in unique(dat[, id.name])) {
      emptyvols <- dat[dat[, id.name]==i, empty.name] * dat[dat[, id.name]==i, paste0(dat.name, '.std')]
      ccvv <- c(0, cumsum(emptyvols)[- length(emptyvols)]) + dat[dat[, id.name]==i, paste0(dat.name, '.std')]
      dat[dat[, id.name]==i, dnn <- paste0(dat.name, '.std.interval')] <- diff(c(0, ccvv))
    }
    
    dat.name <- dnn
  }
  
  return(dat)
}
