# Internal helper functions shared across the calcBg* family.
# Not exported.

# Calculate delta-t vector for production rate calculations.
# dat must already be sorted by id.name and time.name.
# Returns a numeric vector of length nrow(dat), with NA at the first
# observation of each reactor and NA throughout if time class is unrecognized.
calcDeltaT <- function(dat, id.name, time.name, quiet = FALSE) {
  if(inherits(dat[, time.name], c('numeric', 'integer', 'difftime'))) {
    dt <- c(NA_real_, diff(dat[, time.name]))
  } else if(inherits(dat[, time.name], c('POSIXct', 'POSIXlt'))) {
    dt <- c(NA_real_, as.numeric(diff(dat[, time.name]), units = 'days'))
    if(!quiet) message('Rates are per *day*.')
  } else {
    warning('class of time column not recognized, so rates will not be calculated.')
    return(rep(NA_real_, nrow(dat)))
  }
  dt[c(TRUE, dat[, id.name][-1] != dat[, id.name][-nrow(dat)])] <- NA_real_
  dt
}

# Calculate cumulative (cv*) and interval (v*) biogas and methane production.
# If interval = TRUE: input has interval v*, computes cv* via cumsum.
# If interval = FALSE: input has cumulative v*, copies to cv*, computes v* via diff.
# If cmethod = 'total': applies headspace CH4 correction (dat$vhsCH4) to cvCH4
#   and recomputes vCH4. Requires have.comp = TRUE.
calcCumVol <- function(dat, id.name, interval, have.comp, cmethod = 'removed') {
  if(interval) {
    dat$cvBg <- ave(dat$vBg, dat[[id.name]], FUN = cumsum)
    if(have.comp) dat$cvCH4 <- ave(dat$vCH4, dat[[id.name]], FUN = cumsum)
  } else {
    dat$cvBg <- dat$vBg
    if(have.comp) dat$cvCH4 <- dat$vCH4
    dat$vBg  <- ave(dat$cvBg,  dat[[id.name]], FUN = function(x) diff(c(0, x)))
    if(have.comp) dat$vCH4 <- ave(dat$cvCH4, dat[[id.name]], FUN = function(x) diff(c(0, x)))
  }
  if(cmethod == 'total' && have.comp) {
    dat$cvCH4 <- dat$cvCH4 + dat$vhsCH4
    dat$vCH4  <- ave(dat$cvCH4, dat[[id.name]], FUN = function(x) diff(c(0, x)))
  }
  dat
}

# Calculate production rates.
# dt must be a vector of time differences aligned row-for-row with dat
# (as returned by calcDeltaT). Division is fully vectorized; no loop needed.
calcRates <- function(dat, dt, have.comp) {
  dat$rvBg <- dat$vBg / dt
  if(have.comp) dat$rvCH4 <- dat$vCH4 / dt
  dat
}

# Remove trailing NA rows at the end of each reactor's time series.
# Used to handle wide data where some reactors have fewer observations than others.
# dat must be sorted by id.name and time.name before calling.
trimTrailingNAs <- function(dat, id.name, col.name) {
  if(!any(is.na(dat[, col.name]))) return(dat)
  result <- lapply(split(dat, dat[[id.name]]), function(dd) {
    if(!is.na(dd[nrow(dd), col.name])) return(dd)
    i1  <- which(is.na(dd[, col.name]))
    i1d <- diff(i1)
    i2  <- if(any(i1d > 1)) max(which(i1d > 1)) + 1 else 1
    i3  <- i1[i2]
    dd[-c(i3:nrow(dd)), ]
  })
  do.call(rbind, result)
}
