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
  quiet = FALSE)
{

  # When called with multiple response variables
  if(length(vol.name) > 1) {

    res <- data.frame()
    for (i in seq_along(vol.name)) {

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
          # Merge, excluding vol.name i from left and all others from right
          res <- merge(res[, !names(res) %in% vol.name[i]], sb[, !names(sb) %in% vol.name[c(1:length(vol.name))[-i]]])
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

    res.list <- vector('list', length(when))

    for (i in seq_along(when)) {

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
      res.list[[i]] <- sb

    }
    return(Reduce(rbindf, res.list))
  }


  # Main function

  # Argument checks
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

  vol <- as.data.frame(vol)
  setup <- as.data.frame(setup)

  # Argument revisions
  if (tolower(when) == 'latest') {
    extrap <- TRUE
  }

  # Check for pd when argument
  # First for backward compatibility
  if(length(when) == 1 && when == '1p') when <- '1p3d'
  if(length(when) == 1 && when == '0.5p') when <- '0.5p3d'
  pdwhen <- length(when) == 1 && gsub('[0-9.]', '', when) == 'pd'
  pdnotyet <- NULL

  # Warning on show.rates
  if (!pdwhen & show.rates) {
    if (!missing(when)) {
      warning('You set \"show.rates = TRUE\", so \"when\" argument will be ignored.')
    }
    pdwhen <- TRUE
    when <- '1p1d'
  }

  # Echo response variable
  if(!quiet) message('Response variable: ', vol.name, '.')

  # Check for missing columns in vol
  if(is.numeric(when)) {
    if(any(missing.col <- !c(id.name, time.name, vol.name) %in% names(vol))){
      stop('Specified columns not found in vol: ', paste(c(id.name, time.name, vol.name)[missing.col], collapse=', '), '.')
    }
  } else {
    if(any(missing.col <- !c(id.name, vol.name) %in% names(vol))){
      stop('Specified columns not found in vol: ', paste(c(id.name, vol.name)[missing.col], collapse=', '), '.')
    }
  }

  # Check for missing columns in setup
  if(any(missing.col <- !c(id.name, descrip.name) %in% names(setup))){
    stop('Specified columns not found in setup: ', paste(c(id.name, descrip.name)[missing.col], collapse=', '), '.')
  }

  # Check that inoc.name and norm.name can be found in setup data frame
  if(!is.null(inoc.name) && !inoc.name %in% setup[, descrip.name]) {
    stop('inoc.name "', inoc.name, '" not found in setup$', descrip.name, '.')
  }

  if(!is.null(norm.name) && !norm.name %in% names(setup)) {
    stop('norm.name "', norm.name, '" not found in the column names of setup.')
  }

  if(!is.null(inoc.m.name) && !inoc.m.name %in% names(setup)) {
    stop('inoc.m.name "', inoc.m.name, '" not found in the column names of setup.')
  }

  if(!is.null(inoc.name) & is.null(inoc.m.name)) {
    stop('inoc.m.name must be provided in order to subtract inoculum contribution.')
  }

  # Check for case when 'when' argument > all times
  if((is.numeric(when) | is.integer(when)) && all(when > vol[, time.name])) {
    stop('when argument (', when, ') is > all times in data.')
  }

  # Trim setup based on ids present in both vol and setup
  ids <- intersect(setup[, id.name], vol[, id.name])
  setup <- setup[setup[, id.name] %in% ids, ]

  if(!is.null(inoc.name) && !inoc.name %in% setup[, descrip.name]) {
    stop('inoc.name "', inoc.name, '" no longer in setup after trimming--are reactors present in setup missing in vol?')
  }

  # Remove inoc ids
  if(!is.null(inoc.name)) {
    ids.all <- ids
    ids <- setup[setup[, descrip.name] != inoc.name, id.name]
    ids.inoc <- setup[setup[, descrip.name] == inoc.name, id.name]
  }

  # Check for duplicates
  if(any(duplicated(setup[, id.name]))) {
    stop('Duplicated reactor IDs (', id.name, ' column) in setup data frame! This must be an error.')
  }

  if(any(duplicated(vol[, c(id.name, time.name)]))) {
    stop('Duplicated ID (', id.name, ' column) x time (', time.name, ' column) in vol data frame! This must be an error.')
  }

  # Drop missing values from vol with a warning
  if(any(is.na(vol[, vol.name]))) {
    warning('Missing volume data in vol data frame will be dropped.')
    vol <- vol[!is.na(vol[, vol.name]), ]
  }

  # Interpolate to common time, or select values (when = 'end' or 'meas')
  if(is.numeric(when)) {

    summ1 <- expand.grid(id = ids, time = when)
    names(summ1) <- c(id.name, time.name)

    for(i in ids) {
      dc <- vol[vol[, id.name] == i, ]
      if(nrow(dc) > 1) {
        summ1[summ1[, id.name] == i, vol.name] <- interp(dc[, time.name], dc[, vol.name], time.out = when, method = imethod, extrap = extrap)
      } else {
        if(dc[, time.name] == when) {
          summ1[summ1[, id.name] == i, vol.name] <- dc[, vol.name]
        } else {
          summ1[summ1[, id.name] == i, vol.name] <- NA
          warning('There is only a single ', vol.name, ' value for reactor ', i, ', and it does not match the specified when (', when, '). Interpolation is not possible.')
        }
      }
    }

  } else if(length(when) == 1 && tolower(when) %in% c('end', 'latest')) {

    vol <- vol[order(vol[, id.name], vol[, time.name]), ]
    vol.ids <- vol[vol[, id.name] %in% ids, ]
    summ1 <- do.call(rbind, lapply(split(vol.ids, vol.ids[[id.name]]), tail, 1))
    summ1 <- summ1[, c(id.name, time.name, vol.name)]

    if(tolower(when) == 'latest') {
      summ1[, time.name] <- Inf
    }

  } else if(length(when) == 1 && (when == 'meas' | pdwhen)) {

    if(rate.crit == 'net') {
      summ1 <- vol[vol[, id.name] %in% ids, c(id.name, time.name, vol.name)]
    } else if(rate.crit %in% c('gross', 'total')) {
      summ1 <- vol[vol[, id.name] %in% ids.all, c(id.name, time.name, vol.name)]
    }

  } else  {

    stop('when argument not recognized. Options are numeric or integer vector, \"end\" or \"meas\".')

  }

  # Get all unique times
  times.summ <- unique(summ1[, time.name])

  # Interpolate inoculum-only reactors to all unique times
  if(!is.null(inoc.name)) {

    summ.inoc <- setNames(expand.grid(ids.inoc, times.summ), c(id.name, time.name))

    for(i in ids.inoc) {
      dc <- vol[vol[, id.name] == i, ]
      if(nrow(dc) > 1) {
        summ.inoc[summ.inoc[[id.name]] == i, vol.name] <- interp(dc[, time.name], dc[, vol.name], time.out = times.summ, method = imethod, extrap = extrap)
      } else {
        if(dc[, time.name] == times.summ) {
          summ.inoc[summ.inoc[[id.name]] == i, vol.name] <- dc[, vol.name]
        } else {
          summ.inoc[summ.inoc[[id.name]] == i, vol.name] <- NA
          warning('There is only a single ', vol.name, ' value for reactor ', i, ', and it does not match the specified when (', when, '). Interpolation is not possible.')
        }
      }
    }

    # Check for NAs in inoculum data (probably extrapolation issue)
    if(any(is.na(summ.inoc[, vol.name]))) {
      warning('Missing values in inoculum-only volumes. Did the inoculum-only incubation end before other bottles or before \'when\'? Dropping observation(s). Try extrap = TRUE to retain (but be aware of what this means).')
      summ.inoc <- summ.inoc[!is.na(summ.inoc[, vol.name]), ]
    }

    if(max(summ.inoc[[time.name]]) < max(summ1[, time.name])) {
      warning('Times for the inoculum-only bottles do not extend as far as times for other bottles. See NaNs in output. Select a shorter time to avoid NaNs.')
    }

    # Merge to add inoculum mass
    summ.inoc <- merge(setup[, c(id.name, inoc.m.name)], summ.inoc, by = id.name)

    # Volume contribution per unit inoculum mass
    summ.inoc$vol.mi <- summ.inoc[, vol.name] / summ.inoc[, inoc.m.name]

    # Mean and SE of per-unit-mass inoculum contribution, per time point
    inoc.vol <- do.call(rbind, lapply(times.summ, function(tt) {
      vol.mi <- summ.inoc[summ.inoc[[time.name]] == tt, 'vol.mi']
      if(length(vol.mi) == 1) {
        warning('Only one inoculum-only bottle is present, so reported sd does not include variation within inoculum-only bottles.')
      }
      se <- if(length(vol.mi) > 1) sd(vol.mi)/sqrt(length(vol.mi)) else 0
      setNames(data.frame(tt, mean(vol.mi), se, length(vol.mi)),
               c(time.name, 'vol.mi.mn', 'vol.mi.se', 'n'))
    }))
    inoc.vol$rsd.inoc <- 100 * inoc.vol$vol.mi.se / inoc.vol$vol.mi.mn * sqrt(inoc.vol$n)

  }

  # Add inoculum mass and substrate VS to sample data
  summ1 <- merge(setup, summ1, by = id.name)

  if(!is.null(inoc.name)) {

    summ1 <- merge(summ1, inoc.vol, by = time.name)

    summ1[, paste0(vol.name, '.tot')]  <- summ1[, vol.name]
    summ1[, paste0(vol.name, '.inoc')] <- summ1$vol.mi.mn * summ1[, inoc.m.name]
    summ1[, 'fv.inoc'] <- summ1[, paste0(vol.name, '.inoc')] / summ1[, paste0(vol.name, '.tot')]

    summ1[, vol.name] <- summ1[, vol.name] - summ1$vol.mi.mn * summ1[, inoc.m.name]

    # SE in inoculum volume contribution for error propagation
    summ1[, 'se.inoc'] <- summ1$vol.mi.se * summ1[, inoc.m.name]

  } else {

    summ1[, 'se.inoc'] <- 0

  }

  # Messages about inoculum
  if(!is.null(inoc.name) && inoc.name %in% setup[, descrip.name]) {
    if(!quiet) message('Inoculum contribution subtracted based on setup$', inoc.m.name, '.')
  } else {
    if(!quiet) message('Inoculum contribution not subtracted.')
  }

  # If selected, find times where rate drops below cutoff%/d of cumulative
  if(length(when) == 1 && pdwhen) {

    cutoff <- as.numeric(gsub('p.+$', '', when))/100

    s1times.list <- vector('list', 0)
    summ1$rrvCH4 <- NA

    # ii: ids.all for rate.crit %in% c('gross', 'total'), otherwise substrate ids only
    ii <- unique(summ1[, id.name])

    summ1 <- summ1[order(summ1[, id.name], summ1[, time.name]), ]
    for(i in ii) {
      dd <- summ1[summ1[, id.name] == i, ]

      if(rate.crit == 'net') {
        rr <- c(NA, diff(dd[, vol.name])/diff(dd[, time.name])) / dd[, vol.name]
      } else if(rate.crit %in% c('gross', 'total')) {
        rr <- c(NA, diff(dd[, paste0(vol.name, '.tot')])/diff(dd[, time.name])) / dd[, paste0(vol.name, '.tot')]
      }

      summ1[summ1[, id.name] == i, 'rrvCH4'] <- signif(100*rr, 5)
    }

    if(show.rates) {
      if(!quiet) message('Returning output with calculated relative production rates.')
      if (!missing(norm.name)) {
        if(!quiet) warning('Volume values were *not* normalized!\n  To get normalized values, run with show.rates = FALSE (default).')
      }
      if (!show.obs) {
        if(!quiet) warning('Means are not calculated!')
      }
      summ1 <- summ1[order(summ1[, id.name], summ1[, time.name]), ]
      return(summ1)
    }

    for(i in ii) {
      dd <- summ1[summ1[, id.name] == i, ]

      rr <- dd$rrvCH4/100
      tt <- dd[, time.name]

      i1 <- which(rr <= cutoff)
      i1d <- diff(i1)

      if(length(i1d) > 0 & any(i1d > 1)) {
        i2 <- max(which(i1d > 1)) + 1
      } else {
        i2 <- 1
      }

      dur <- as.numeric(gsub('^.+p(.+)d', '\\1', when))
      i3 <- i1[i2]
      i3 <- which(tt - tt[i3 - 1] >= dur)[1]

      if(!is.na(i3)) {
        ss <- dd[i3, ]
      } else {
        ss <- dd[nrow(dd), ]
        pdnotyet <- c(pdnotyet, i)
      }
      s1times.list <- c(s1times.list, list(ss))
    }
    s1times <- do.call(rbind, s1times.list)

    # Even out times among replicates of the same description
    pieces <- list()
    idx <- 0
    for(i in unique(s1times[, descrip.name])) {

      tt <- max(s1times[s1times[, descrip.name] == i, time.name], when.min)

      for(j in unique(summ1[summ1[, descrip.name] == i, id.name])) {

        if(max(summ1[summ1[, id.name] == j, time.name]) < tt) {
          tt <- max(summ1[summ1[, id.name] == j, time.name])
          pdnotyet <- c(pdnotyet, j)
        }

        ss <- summ1[summ1[, id.name] == j & summ1[, time.name] >= tt, ]
        if(nrow(ss) == 0) stop('when = "xpyd" problem. Call function again with show.rates = TRUE')
        idx <- idx + 1
        pieces[[idx]] <- ss[1, ]
      }

    }

    summ1 <- do.call(rbind, pieces)

    if(rate.crit %in% c('gross', 'total')) {
      summ1 <- summ1[summ1[, id.name] %in% ids, ]
    }

  }

  # Normalization
  if(!is.null(norm.name)) {

    if(!is.null(norm.se.name)) {
      summ1[, paste0(vol.name, '.se')] <- summ1[, vol.name]/summ1[, norm.name] * summ1[, norm.se.name]/summ1[, norm.name]
    } else {
      summ1[, paste0(vol.name, '.se')] <- 0
    }

    summ1[, vol.name]    <- summ1[, vol.name]    / summ1[, norm.name]
    summ1[, 'se.inoc']   <- summ1[, 'se.inoc']   / summ1[, norm.name]

    if(!is.null(inoc.name) && inoc.name %in% setup[, descrip.name]) {
      summ1[, paste0(vol.name, '.tot')]  <- summ1[, paste0(vol.name, '.tot')]  / summ1[, norm.name]
      summ1[, paste0(vol.name, '.inoc')] <- summ1[, paste0(vol.name, '.inoc')] / summ1[, norm.name]
    }
  } else {
    summ1[, paste0(vol.name, '.se')] <- 0
  }

  # Calculate means and SE for a summary
  if(!show.obs) {

    grp_keys <- unique(summ1[, c(descrip.name, time.name)])
    summ2_list <- vector('list', nrow(grp_keys))

    for(k in seq_len(nrow(grp_keys))) {
      i   <- grp_keys[k, descrip.name]
      j   <- grp_keys[k, time.name]
      ddd <- summ1[summ1[, descrip.name] == i & summ1[, time.name] == j, ]
      n   <- sum(!is.na(ddd[, vol.name]))

      row <- grp_keys[k, , drop = FALSE]
      row[, 'mean'] <- mean(ddd[, vol.name], na.rm = TRUE)
      row[, 'se']   <- sqrt((sd(ddd[, vol.name], na.rm = TRUE)/sqrt(n))^2 +
                            (sqrt(sum(ddd[, 'se.inoc']^2)/n))^2 +
                            (sqrt(sum(ddd[, paste0(vol.name, '.se')]^2)/n))^2)
      row[, 'sd']   <- row[, 'se'] * sqrt(n)
      row[, 'n']    <- n

      if(!is.null(inoc.name)) {
        row[, 'rsd.inoc'] <- ddd[1, 'rsd.inoc']
        row[, 'fv.inoc']  <- mean(ddd[, 'fv.inoc'], na.rm = TRUE)
        row[, 'se1']      <- sd(ddd[, vol.name], na.rm = TRUE) / sqrt(n)
        row[, 'se2']      <- sqrt(sum(ddd[, 'se.inoc']^2) / n)
        row[, 'se3']      <- sqrt(sum(ddd[, paste0(vol.name, '.se')]^2) / n)
      }

      row[, 'rate.crit.met'] <- if(!is.null(pdnotyet)) !any(ddd[, id.name] %in% pdnotyet) else TRUE

      summ2_list[[k]] <- row
    }

    summ2 <- do.call(rbind, summ2_list)
    rownames(summ2) <- NULL

  } else {
    summ2 <- summ1[order(summ1[, descrip.name], summ1[, id.name], summ1[, time.name]), ]
  }

  # Message about normalization
  if(!missing(norm.name)) {
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

    summ2 <- summ2[, s2cols]

  }

  # Sort result
  if(sort) {
    if(show.obs) {
      summ2 <- summ2[order(summ2[, descrip.name], summ2[, id.name], summ2[, time.name]), ]
    } else {
      summ2 <- summ2[order(summ2[, descrip.name], summ2[, time.name]), ]
    }
  } else {
    descrip.order <- seq_along(unique(setup[, descrip.name]))
    names(descrip.order) <- setup[!duplicated(setup[, descrip.name]), descrip.name]
    summ2 <- summ2[order(descrip.order[as.character(summ2[, descrip.name])], summ2[, time.name]), ]
  }

  rownames(summ2) <- 1:nrow(summ2)

  if(is.null(pdnotyet)) {
    pdnotyet <- ''
  } else {
    warning('You selected ', when, ' option for \"when\" argument but there are no observations that meet the criterion for the following bottles. Instead, the latest time was selected. ', paste(pdnotyet, collapse = ', '))
  }

  attr(summ2, 'rate.not.met') <- pdnotyet
  return(summ2)

}
