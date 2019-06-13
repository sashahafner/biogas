summBg <- function(
  vol,
  setup,
  id.name = 'id',
  time.name = 'time',
  descrip.name = 'descrip',
  inoc.name = NULL,
  inoc.m.name = NULL,
  norm.name = NULL,
  norm.se.name = NULL,
  vol.name = 'cvCH4',
  imethod = 'linear',
  extrap = FALSE,
  when = 30,
  when.min = 0,
  rate.crit = 'net',
  show.obs = FALSE, 
  show.rates = FALSE, 
  show.more = FALSE,
  sort = TRUE,
  set.name = 'set', 
  quiet = FALSE)
{

  # For "vectorized" calls, lapply-like behavior
  if(class(vol)[1] == 'list') {

    # Check reserved names
    # NTS: need to add more reserved names to check
    if (any(set.name == c(names(vol), names(setup), c('mean', 'sd', 'se', 'n')))) {
      stop('Argument set.name matches another column name')
    }

    if(class(setup)[1] == 'data.frame') {

      res <- data.frame()

      for (i in 1:length(vol)) {

        sb <- summBg(vol = vol[[i]],
                     setup = setup,
                     id.name = id.name,
                     time.name = time.name,
                     descrip.name = descrip.name,
                     inoc.name = inoc.name,
                     inoc.m.name = inoc.m.name,
                     norm.name = norm.name,
                     norm.se.name = norm.se.name,
                     vol.name = vol.name,
                     imethod = imethod,
                     extrap = extrap,
                     when = when,
                     when.min = when.min,
                     rate.crit = rate.crit,
                     show.obs = show.obs,
                     show.rates = show.rates,
                     show.more = show.more,
                     sort = sort,
                     quiet = quiet)

        # Add experiment as first column
        sb[, set.name] <- names(vol)[i]
        sb <- sb[, c(ncol(sb), 1:(ncol(sb) - 1))]
        res <- rbind(res, sb)

      }

      return(res)

    } else {

      stop('Error  xueru187')

    }

  }

  # When called with multiple response variables
  if(length(vol.name) > 1) {

    # Loop through all, but output structure depends on show.obs
    res <- data.frame()
    for (i in 1:length(vol.name)) {

      sb <- summBg(vol = vol,
                   setup = setup,
                   id.name = id.name,
                   time.name = time.name,
                   descrip.name = descrip.name,
                   inoc.name = inoc.name,
                   inoc.m.name = inoc.m.name,
                   norm.name = norm.name,
                   norm.se.name = norm.se.name,
                   vol.name = vol.name[i],
                   imethod = imethod,
                   extrap = extrap,
                   when = when,
                   rate.crit = rate.crit,
                   show.obs = show.obs,
                   show.rates = show.rates,
                   show.more = show.more,
                   sort = sort,
                   quiet = quiet)


      if (show.obs) {
        # Drop columns that cannot be merged (same name, different values for each vol.name)
        sb <- sb[, !names(sb) %in% c('vol.mi.mn', 'vol.mi.se', 'rsd.inoc', 'fv.inoc', 'se.inoc')]
        if (i == 1) {
          res <- sb
        } else {
          # Merge, excluding vol.name i (current) from left one (cumulative data frame) and all others from right one (new one)
          res <- merge(res[, !names(res) %in% vol.name[i]], sb[, !names(sb) %in% vol.name[c(1:length(vol.name))[-i]]])
          #res[, vol.name[i]] <- sb[, vol.name[i]]
        }
      } else {
        sb[, 'vol.name'] <- vol.name[i]
        sb <- sb[, c(ncol(sb), 1:(ncol(sb) - 1))]
        res <- rbind(res, sb)
      }

    }

    return(res)
  }

  # When called with list or vector for when
  if(length(when) > 1) {

    # Loop through all, but output structure depends on show.obs
    res <- data.frame()

    for (i in 1:length(when)) {

      sb <- summBg(vol = vol,
                   setup = setup,
                   id.name = id.name,
                   time.name = time.name,
                   descrip.name = descrip.name,
                   inoc.name = inoc.name,
                   inoc.m.name = inoc.m.name,
                   norm.name = norm.name,
                   norm.se.name = norm.se.name,
                   vol.name = vol.name,
                   imethod = imethod,
                   extrap = extrap,
                   when = when[[i]],
                   rate.crit = rate.crit,
                   show.obs = show.obs,
                   show.rates = show.rates,
                   show.more = show.more,
                   sort = sort,
                   quiet = quiet)

      sb[, 'when'] <- when[[i]]
      sb <- sb[, c(ncol(sb), 1:(ncol(sb) - 1))]
      res <- rbindf(res, sb)

    }
    return(res)
  }


  # Main function

  # Argument checks~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  checkArgClassValue(vol, 'data.frame')
  checkArgClassValue(setup, 'data.frame')
  checkArgClassValue(id.name, 'character')
  checkArgClassValue(time.name, c('character', 'NULL'))
  checkArgClassValue(descrip.name, c('character', 'NULL'))
  checkArgClassValue(inoc.name, c('character', 'NULL'))
  checkArgClassValue(norm.name, c('character', 'NULL'))
  checkArgClassValue(inoc.m.name, c('character', 'NULL'))
  checkArgClassValue(vol.name, 'character')
  # Skip imethod, since it is checked in interp()
  checkArgClassValue(extrap, 'logical')
  checkArgClassValue(when, c('numeric', 'integer', 'character', 'NULL'))
  checkArgClassValue(rate.crit, 'character', c('net', 'gross', 'total'))
  checkArgClassValue(show.obs, 'logical')
  checkArgClassValue(sort, 'logical')
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Check for pd when argument
  # First for backward compatability
  if(length(when) == 1 && when == '1p') when <- '1p3d'
  if(length(when) == 1 && when == '0.5p') when <- '0.5p3d'
  pdwhen <- length(when) == 1 && gsub('[0-9.]', '', when) == 'pd'
  pdnotyet <- NULL

  # Warning on show.rates
  if(!pdwhen & show.rates) {
      warning('You set \"show.rates = TRUE\", so \"when\" argument will be ignored.')
      pdwhen <- TRUE
      when <- '1p1d'
  }

  # Echo response variable
  if(!quiet) message('Response variable (volume) is ', deparse(substitute(vol)), '$', vol.name, '.')

  # Check for missing columns in vol
  if(class(when) %in% c('numeric', 'integer')) {
    if(any(missing.col <- !c(id.name, time.name, vol.name) %in% names(vol))){
      stop('Specified columns in vol data frame (', deparse(substitute(vol)), ') not found: ', c(id.name, time.name, vol.name)[missing.col], '.')
    } 
  } else { # when is 'end' or 'meas'
    if(any(missing.col <- !c(id.name, vol.name) %in% names(vol))){
      stop('Specified columns in vol data frame (', deparse(substitute(vol)), ') not found: ', c(id.name, vol.name)[missing.col], '.')
    } 
  }

  # Check for missing columns in setup
  if(any(missing.col <- !c(id.name, descrip.name) %in% names(setup))){
    stop('Specified columns in setup data frame (', deparse(substitute(setup)), ') not found: ', c(id.name, descrip.name)[missing.col], '.')
  } 

  # Check that inoc.name and norm.name can be found in setup data frame
  if(!is.null(inoc.name) && !inoc.name %in% setup[, descrip.name]) {
    stop('inoc.name ', deparse(substitute(inoc.name)), ' not found in ', deparse(substitute(setup)), '$', descrip.name, '.')
  }

  if(!is.null(norm.name) && !norm.name %in% names(setup)) {
    stop('norm.name ', deparse(substitute(norm.name)), ' not found in the column names of ', deparse(substitute(setup)), '.')
  }

  # And inoc.m.name
  if(!is.null(inoc.m.name) && !inoc.m.name %in% names(setup)) {
    stop('inoc.m.name ', deparse(substitute(inoc.m.name)), ' not found in the column names of ', deparse(substitute(setup)), '.')
  }

  # Problem if inoc.name is given but inoc.m.name is not
  if(!is.null(inoc.name) & is.null(inoc.m.name)) {
    stop('inoc.m.name must be provided in order to subtract inoculumn contribution.')
  }

  # Check for case when 'when' argument > all times
  if((is.numeric(when) | is.integer(when)) && all(when > vol[, time.name])) {
    stop('when argument (', when, ') is > all times in data.')

  }

  # Add other checks here

  # Trim setup based on ids and check again for inoc.name and norm.name~~~~~~~~~~~~~~~~~~~
  # Find reactor/bottle IDs present in both vol and setup
  ids <- intersect(setup[, id.name], vol[, id.name])

  setup <- setup[setup[, id.name] %in% ids, ]

  if(!is.null(inoc.name) && !inoc.name %in% setup[, descrip.name]) {
    stop('inoc.name ', deparse(substitute(inoc.name)), ' no longer in setup after trimming--are reactors present in setup missing in vol?')
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Remove inoc ids
  if(!is.null(inoc.name)) {
    ids.all <- ids
    ids <- setup[setup[, descrip.name]!=inoc.name, id.name]
    ids.inoc <- setup[setup[, descrip.name]==inoc.name, id.name]
  }

  # Check for duplicates in setup and vol~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(any(duplicated(setup[, id.name]))) {
    stop('Duplicated reactor IDs (', id.name, ' column) in setup data frame! This must be an error.')
  }

  if(any(duplicated(vol[, c(id.name, time.name)]))) {
    stop('Duplicated ID (', id.name, ' column) x time (', time.name, ' column) in vol data frame! This must be an error.')
  }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Drop missing values from vol with a warning
  if(any(is.na(vol[, vol.name]))) {
    warning('Missing volume data in vol data frame will be dropped.')
    vol <- vol[!is.na(vol[, vol.name]), ]
  }

  # Interpolate cvCH4 to common time for each reactor~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Or select values for analysis (when = 'end' or 'meas')

  if(class(when) %in% c('numeric', 'integer')) {
    summ1 <- expand.grid(id = ids, time = when)
    names(summ1) <- c(id.name, time.name)

    # Then interpolate
    for(i in ids) {
      dc <- vol[vol[, id.name]==i, ]
      # Interpolate if more than one value is present

      if(nrow(dc)>1) {
        summ1[summ1[, id.name]==i, vol.name] <- interp(dc[, time.name], dc[, vol.name], time.out = when, method = imethod, extrap = extrap)
      } else {
	if(dc[, time.name]==when) { # `when` argument matches the single time present
          summ1[summ1[, id.name]==i, vol.name] <- dc[, vol.name]
	} else {
          summ1[summ1[, id.name]==i, vol.name] <- NA
      	  warning('There is only a single ', vol.name, ' value for reactor ', i,', and it does not match the specified when (', when, '). Interpolation is not possible.')
	}
      }

    }

  } else if(length(when) == 1 && tolower(when) == 'end') { # User just wants to use latest values of volume

    summ1 <- data.frame(id = ids, time = NA, vol = NA)
    names(summ1) <- c(id.name, time.name, vol.name)

    # Sort, in order to find latest values
    vol <- vol[order(vol[, id.name], vol[, vol.name]), ]

    for(i in ids) {
      dc <- vol[vol[, id.name]==i, ]
      # Select the last row from sorted data frame
      summ1[summ1[, id.name]==i, c(time.name, vol.name)] <- dc[nrow(dc), c(time.name, vol.name)]
    }

  #} else if(length(when) == 1 && when %in% c('meas', '1p', '0.5p')) { # Return values for all measurement times, which may differ among reactors
  } else if(length(when) == 1 && (when == 'meas' | pdwhen)) { 

    # Only substrate ids for net, all (include inoculum) for gross
    if(rate.crit == 'net') {
      summ1 <- vol[vol[, id.name] %in% ids, c(id.name, time.name, vol.name)]
    } else if(rate.crit %in% c('gross', 'total')) {
      summ1 <- vol[vol[, id.name] %in% ids.all, c(id.name, time.name, vol.name)]
    }

  } else  {

    stop('when argument not recognized. Options are numeric or integer vector, \"end\" or \"meas\".')

  
  }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Get all unique times
  times.summ <- unique(summ1[, time.name])

  # Work with inoculum data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Now interpolate inoculum-only reactors to all unique times
  if(!is.null(inoc.name)) {

    summ.inoc <- expand.grid(id = ids.inoc, time = times.summ)

    # Then interpolate inoculum production (each inoc reactor) to each time
    for(i in ids.inoc) {

      dc <- vol[vol[, id.name]==i, ]

      # Interpolate if more than one value is present
      if(nrow(dc)>1) {
        summ.inoc[summ.inoc$id==i, vol.name] <- interp(dc[, time.name], dc[, vol.name], time.out = times.summ, method = imethod, extrap = extrap)
      } else {

	if(dc[, time.name]==times.summ) { # `when` argument matches the single time present
          summ.inoc[summ.inoc$id==i, vol.name] <- dc[, vol.name]
	} else {
          summ.inoc[summ.inoc$id==i, vol.name] <- NA
      	  warning('There is only a single ', vol.name, ' value for reactor ', i,', and it does not match the specified when (', when, '). Interpolation is not possible.')
	}

      }

    }

    # Check for NAs in inoculum data (probably extrapolation issue)
    if(any(is.na(summ.inoc[, vol.name]))) {
      warning('Missing values in inoculum-only volumes. Did the inoculum-only incubation end before other bottles or before \'when\'? Dropping observation(s). Try extrap = TRUE to retain (but be aware of what this means).')
      summ.inoc <- summ.inoc[!is.na(summ.inoc[, vol.name]), ]
    }

    # See if latest times have been dropped/are not available
    if(max(summ.inoc$time) < max(summ1[, time.name])) {
      warning('Times for the inoculum-only bottles do not extend as far as times for other bottles. See NaNs in output. Select a shorter time to avoid NaNs.')
    }

    # Merge to add mass inoculum and VS in substrate
    # Merge only necessary columns!
    summ.inoc <- merge(setup[, c(id.name, inoc.m.name)], summ.inoc, by.x = id.name, by.y = 'id')

    # Volume contribution per unit inoculum mass
    summ.inoc$vol.mi <- summ.inoc[, vol.name]/summ.inoc[, inoc.m.name]

    # Mean and se volume contribution per unit inoc mass
    inoc.vol <- data.frame()

    for(i in times.summ) {
      vol.mi <- summ.inoc[summ.inoc$time == i, 'vol.mi']
      # Calculate se only if there is more than one observation
      if(length(vol.mi) > 1) {
        ss <- sd(vol.mi)/sqrt(length(vol.mi))
      } else {
        ss <- 0
        warning('Only one inoculum-only bottle is present, so reported sd does not include variation within inoculum-only bottles.')
      }
      inoc.vol <- rbind(inoc.vol, c(time = i, mn = mean(vol.mi), s = ss, n = length(vol.mi)))
    }

    names(inoc.vol) <- c(time.name, 'vol.mi.mn', 'vol.mi.se', 'n')
    inoc.vol$rsd.inoc <- 100*inoc.vol$vol.mi.se/inoc.vol$vol.mi.mn*sqrt(inoc.vol$n)
    # inoc.vol has mean and se vol per unit mass inoc for all times

  }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Samples
  # Add mass of inoculum and VS in substrate
  summ1 <- merge(setup, summ1, by = id.name)

  if(!is.null(inoc.name)) {

    # Merge inoculum normalized volumes with sample data
    summ1 <- merge(summ1, inoc.vol, by = time.name)

    # Calculate and substract inoc contribution
    # Next three for returning additional info when show.rates = TRUE and for rate.crit = gross (??)
    summ1[, paste0(vol.name, '.tot')] <- summ1[, vol.name]
    summ1[, paste0(vol.name, '.inoc')] <- summ1$vol.mi.mn*summ1[, inoc.m.name]
    summ1[, 'fv.inoc'] <- summ1[, paste0(vol.name, '.inoc')]/summ1[, paste0(vol.name, '.tot')]

    # Correct vol for inoculum
    summ1[, vol.name] <- summ1[, vol.name] - summ1$vol.mi.mn*summ1[, inoc.m.name]

    # Add se in volume produced by inoculum for use below in error propagation
    summ1[, 'se.inoc'] <- summ1$vol.mi.se*summ1[, inoc.m.name]

  } else {

    # NTS: How did I handle this before 10 Feb 2016?
    summ1[, 'se.inoc'] <- 0

  }

  # If selected, find times where rate drops below 1%/d of cumulative
  if(length(when) == 1 && pdwhen) { 

    # Get cutoff 
    cutoff <- as.numeric(gsub('p.+$', '', when))/100
##    if(when == '1p') {
##      cutoff <- 0.01 
##    } else {
##      cutoff <- 0.005
##    }

    # Find time when rvCH4 <= 1% of cvCH4
    s1times <- NULL
    summ1$rrvCH4 <- NA

    # Calculate relative rates
    ii <- unique(summ1[, id.name]) # Because is ids.all for rate.crit %in% c('gross', 'total') otherwise ids (substrate only)

    for(i in ii) {
      dd <- summ1[summ1[, id.name] == i, ]
      dd <- dd[order(dd[, time.name]), ]

      if(rate.crit == 'net') {
        rr <- c(NA, diff(dd[, vol.name])/diff(dd[, time.name]))/dd[, vol.name]
      } else if(rate.crit %in% c('gross', 'total')) {
        rr <- c(NA, diff(dd[, paste0(vol.name, '.tot')])/diff(dd[, time.name]))/dd[, paste0(vol.name, '.tot')]
      }

      # Add rates to summ1 only for exporting with show.rates = TRUE
      summ1[summ1[, id.name] == i, 'rrvCH4'] <- signif(100*rr, 4)
    }

    # Return observations here (early to avoid problem in next 2 blocks--see error messages)
    if(show.rates) {
      summ1 <- summ1[order(summ1[, id.name], summ1[, time.name]), ]
      return(summ1)
    }

    # Back to working with rates (after show.rates option above)
    for(i in ii) {
      dd <- summ1[summ1[, id.name] == i, ]

      rr <- dd$rrvCH4/100
      tt <- dd[, time.name]

      # Find rates < 1%
      i1 <- which(rr <= cutoff)

      # That are consecutive
      # If i1 is length 1, this is integer(0)
      i1d <- diff(i1)

      # That are uninterupted by a high rate
      if(length(i1d) > 0 & any(i1d > 1)) {
        i2 <- max(which(i1d > 1)) + 1 
      } else {
        i2 <- 1
      }

      # Take first following time at least dur (usually 3 d) after obs preceeding first obs below 1% (this is correct!--think about production for first obs starting just after preceeding obs, so 3 d count should start then
      # But, limitation of this approach is that a single observation < 1% can end trial (as long as it is at least 3 d after previous)
      # Users should avoid case when returned 1p time = final time in trial
      dur <- as.numeric(gsub('^.+p(.+)d', '\\1', when))
      i3 <- i1[i2]
      i3 <- which(tt - tt[i3 - 1] >= dur)[1]

      if(!is.na(i3)) {
        ss <- dd[i3, ]
      } else {
        #stop('You selected ', when, ' option for \"when\" argument but there are no observations that meet the criterion for id ', i, ' (and possibly others). Either use a fixed time for \"when\" or remove this id. Leave when = ', when, ' and set show.rates = TRUE to check rates for all bottles.')
        ##ss <- dd[nrow(dd), ]
        ##s1times <- rbind(s1times, ss)
        # Set to latest time, but keep track of this
        ss <- dd[nrow(dd), ]
        pdnotyet <- c(pdnotyet, i)
      }
      s1times <- rbind(s1times, ss)

    }

    ## Check to see if all bottles met specified criterion
    #if(any(s1times[, time.name] == -Inf)) {
    #    message('You selected ', when, ' option for \"when\" argument but there are no observations that meet the criterion for these bottles: ', paste0(s1times[s1times[, time.name] == -Inf, id.name], collapse = ', '), '. Either change \"when\" argument or remove this (these) bottles. Set show.rates = TRUE to check rates for all bottles.')
    #    return(invisible('Rate criterion not met'))
    #}

    # Check for different times for bottles with same descrip
    summ1temp <- data.frame()

    #if(rate.crit %in% c('gross', 'total')) {
    #  tt <- max(25, max(s1times[, time.name]))
    #}

    for(i in unique(s1times[, descrip.name])) {

      #if(rate.crit == 'net') {
        tt <- max(s1times[s1times[, descrip.name] == i, time.name], when.min)
      #} 

      for(j in unique(summ1[summ1[, descrip.name] == i, id.name])) {

        # Check to make sure measured time extends far enough (i.e., trial did not end too early), if not, set to max for this rep
        if(max(summ1[summ1[, id.name] == j, time.name]) < tt) {
          tt <- max(summ1[summ1[, id.name] == j, time.name])
          pdnotyet <- c(pdnotyet, j)
        }

        # Select times >= max time for this decrip.name level
        ss <- summ1[summ1[, id.name] == j & summ1[, time.name] >= tt, ] # NTS is this missing [1] to select only the first time that >= tt?
        if(length(ss) == 0) stop('when = "xpyd" problem. Re-run function with show.rates = TRUE')
        ss <- ss[1, ]
        summ1temp <- rbind(summ1temp, ss)
      }

    }

    summ1 <- summ1temp

    # Drop inoculum if present
    if(rate.crit %in% c('gross', 'total')) {
      summ1 <- summ1[summ1[, id.name] %in% ids, ]
    }

  } 

  # Normalization
  if(!is.null(norm.name)) { 

    # First calculate se on normalized volume based on se of VS
    if(!is.null(norm.se.name)) {
      summ1[, paste0(vol.name,'.se')] <- summ1[, vol.name]/summ1[, norm.name] * summ1[, norm.se.name]/summ1[, norm.name]
    # Original approach nearly equivalent to calculate relative error in norm.name and apply it directly (i.e., 10% for norm.name = 10% for vol.name)
      #summ1[, paste0(vol.name,'.sd')] <- (summ1[, vol.name]/(summ1[, norm.name] - summ1[, norm.sd.name]) - 
      #                                    summ1[, vol.name]/(summ1[, norm.name] + summ1[, norm.sd.name]))/2
    } else {
      summ1[, paste0(vol.name,'.se')] <- 0
    }

    # Normalize remaining vol by norm.name (typically by substrate VS)
    summ1[, vol.name] <- summ1[, vol.name]/summ1[, norm.name]

    # Normalize se contribution from inoc by the same value
    summ1[, 'se.inoc'] <- summ1[, 'se.inoc']/summ1[, norm.name]

    # Next two lines only for returning additional info when show.obs = TRUE
    # Only have the .tot and .inoc columns when inoc is subtracted out
    if(!is.null(inoc.name) && inoc.name %in% setup[, descrip.name]) { 
      summ1[, paste0(vol.name, '.tot')] <- summ1[, paste0(vol.name, '.tot')]/summ1[, norm.name]
      summ1[, paste0(vol.name, '.inoc')] <- summ1[, paste0(vol.name, '.inoc')]/summ1[, norm.name]
    }
  } else {
      summ1[, paste0(vol.name,'.se')] <- 0
  }

  # Calculate means and se for a summary
  if(!show.obs) {
    # Summarize by description
    summ2 <- unique(summ1[, c(time.name, descrip.name)]) # NTS: may want to put time second

    for(i in unique(summ1[, descrip.name])){
      dd <- summ1[summ1[, descrip.name]==i, ]
      for(j in unique(dd[, time.name])) {
        ddd <- dd[dd[, time.name]==j, ]
        summ2[summ2[, descrip.name]==i & summ2[, time.name]==j, 'mean'] <- mean(na.omit(ddd[, vol.name]))
        summ2[summ2[, descrip.name]==i & summ2[, time.name]==j, 'se'] <- sqrt((sd(na.omit(ddd[, vol.name]))/sqrt(nrow(ddd)))^2 + 
                                                                              ##mean(ddd[, 'sd.inoc'])^2 + 
                                                                              ##mean(ddd[, paste0(vol.name,'.sd')])^2) 
                                                                              (sqrt(sum(ddd[, 'se.inoc']^2)/nrow(ddd)))^2 + 
                                                                              (sqrt(sum(ddd[, paste0(vol.name,'.se')]^2)/nrow(ddd)))^2) 
        summ2[summ2[, descrip.name]==i & summ2[, time.name]==j, 'sd'] <- summ2[summ2[, descrip.name]==i & summ2[, time.name]==j, 'se']*sqrt(nrow(ddd))
        summ2[summ2[, descrip.name]==i & summ2[, time.name]==j, 'n'] <- sum(!is.na(ddd[, vol.name]))  
	      if(!is.null(inoc.name)) {
          summ2[summ2[, descrip.name]==i & summ2[, time.name]==j, 'rsd.inoc'] <- ddd[1, 'rsd.inoc']
          summ2[summ2[, descrip.name]==i & summ2[, time.name]==j, 'fv.inoc'] <- mean(na.omit(ddd[, 'fv.inoc']))
          summ2[summ2[, descrip.name]==i & summ2[, time.name]==j, 'se1'] <- sd(na.omit(ddd[, vol.name]))/sqrt(nrow(ddd))
	        ##summ2[summ2[, descrip.name]==i & summ2[, time.name]==j, 'sd2'] <- mean(ddd[, 'sd.inoc'])
          ##summ2[summ2[, descrip.name]==i & summ2[, time.name]==j, 'sd2'] <- mean(ddd[, 'sd.inoc'])
          summ2[summ2[, descrip.name]==i & summ2[, time.name]==j, 'se2'] <- sqrt(sum(ddd[, 'se.inoc']^2)/nrow(ddd))
          summ2[summ2[, descrip.name]==i & summ2[, time.name]==j, 'se3'] <- sqrt(sum(ddd[, paste0(vol.name,'.se')]^2)/nrow(ddd))
	      }

        if(!is.null(pdnotyet)) {
          summ2[summ2[, descrip.name]==i & summ2[, time.name]==j, 'rate.crit.met'] <- !any(ddd[, id.name] %in% pdnotyet)
        } else {
          summ2[summ2[, descrip.name]==i & summ2[, time.name]==j, 'rate.crit.met'] <- TRUE
        }
      }
    }
  } else { # If show.obs = TRUE, just return individual observations
    #summ1$sd.inoc <- NULL
    summ2 <- summ1[order(summ1[, descrip.name], summ1[, id.name], summ1[, time.name]), ]
  }


  # More messages~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Messages about inoculum 
  if(!is.null(inoc.name) && inoc.name %in% setup[, descrip.name]) { # Inoculum contribution subtracted
    #message('Inoculum contribution subtracted based on ', deparse(substitute(setup.orig)), '$', inoc.m.name, '.') 
    if(!quiet) message('Inoculum contribution subtracted based on setup$', inoc.m.name, '.') 
  } else {
      if(!quiet) message('Inoculum contribution not subtracted.') 
  }

  # Message about normalization
  if(!is.null(norm.name)) { 
    #message('Response normalized by ', deparse(substitute(setup)), '$', norm.name, '.')
    if(!quiet) message('Response normalized by setup$', norm.name, '.')
  } else {
    if(!quiet) message('No normalization by substrate mass.')
  }

  # Select columns
  s2cols <- c(descrip.name, time.name, 'mean', 'se', 'sd', 'n')
  if(!show.obs) {

    if(show.more) {
      s2cols <- c(s2cols, 'rsd.inoc', 'fv.inoc', 'se1', 'se2', 'se3')
    } 
    
    if(pdwhen) {
      s2cols <- c(s2cols, 'rate.crit.met')
    }

    summ2 <- summ2[ , s2cols]

  } 


  # Sort result
  if(sort) {
    if(show.obs) {
      summ2 <- summ2[order(summ2[, descrip.name], summ2[, id.name], summ2[, time.name]), ]
    } else {
      summ2 <- summ2[order(summ2[, descrip.name], summ2[, time.name]), ]
    }
  } else {
    # Get original reactor order from setup
    descrip.order <- 1:length(unique(setup[, descrip.name]))
    names(descrip.order) <- setup[!duplicated(setup[, descrip.name]), descrip.name]

    # Sort
    summ2 <- summ2[order(descrip.order[as.character(summ2[, descrip.name])], summ2[, time.name]), ]
  }

  # Row names
  rownames(summ2) <- 1:nrow(summ2)

  if(is.null(pdnotyet)) {
      pdnotyet <- ''
  } else {
      warning('You selected ', when, ' option for \"when\" argument but there are no observations that meet the criterion for the following bottles. Instead, the latest time was selected. ', paste(pdnotyet, collapse = ', '))
  }

  attr(summ2, 'rate.not.met') <- pdnotyet
  return(summ2)

}
