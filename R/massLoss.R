# General function for bottle mass loss and biogas leakage

massLoss <- function(
  # Main arguments
  dat,                  # Data frame
  time.name,            # Name of time column (for sorting only)
  m.pre.name = NULL,    # Name of column holding the mass before venting
  m.post.name,          # Name of column holding the mass after venting
  id.name               # Name of id to group by
  ) {

  # Check arguments
  checkArgClassValue(dat, 'data.frame')
  checkArgClassValue(time.name, 'character')
  checkArgClassValue(m.pre.name, c('character', 'NULL'))
  checkArgClassValue(m.post.name, 'character')
  checkArgClassValue(id.name, 'character')

  # Sort data frame
  dat <- dat[order(dat[, id.name], dat[, time.name]), ]

  dat$mass.tot  <- ave(dat[, m.post.name], dat[[id.name]], FUN = function(x) c(0, -diff(x)))
  dat$cmass.tot <- ave(dat$mass.tot, dat[[id.name]], FUN = cumsum)

  if (!is.null(m.pre.name)) {
    dat$mass.vent  <- dat[, m.pre.name] - dat[, m.post.name]
    lag.post <- ave(dat[, m.post.name], dat[[id.name]], FUN = function(x) c(NA, x[-length(x)]))
    dat$mass.leak  <- lag.post - dat[, m.pre.name]
    dat$mass.leak[is.na(dat$mass.leak)] <- 0
    dat$cmass.vent <- ave(dat$mass.vent, dat[[id.name]], FUN = cumsum)
    dat$cmass.leak <- ave(dat$mass.leak, dat[[id.name]], FUN = cumsum)
  }

  return(dat)
}
