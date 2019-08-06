cumBgVol <- function(
  # Main arguments
  dat,
  comp = NULL, # Leave NULL for wide and both combos
  temp = NULL,             # Temperature for gas volume measurement
  pres = NULL,             # Pressure for gas volume measurement
  interval = TRUE, # When empty.name is used, there is a mix, and interval is ignored
  data.struct = 'long',    # long, wide, longcombo, widecombo
  id.name = 'id',
  time.name = 'time',
  dat.name = 'vol', # Will be used for first dat column for data.struct = 'wide'
  comp.name = 'xCH4',    # Name of xCH4 column in the data frame. Use for first comp col for data.struct = 'wide'
  # Calculation method and other settings
  cmethod = 'removed',
  imethod = 'linear',       # Method for interpolation
  extrap = FALSE,
  addt0 = TRUE,
  showt0 = TRUE,
  dry = FALSE,
  empty.name = NULL, # Column name for binary/logical column for when cum vol was reset to zero
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
){
  
  # Check arguments
  checkArgClassValue(dat, 'data.frame')
  checkArgClassValue(comp, c('data.frame', 'integer', 'numeric', 'NULL'))
  checkArgClassValue(temp, c('integer', 'numeric', 'character', 'NULL'))
  checkArgClassValue(pres, c('integer', 'numeric', 'character', 'NULL'))
  checkArgClassValue(interval, 'logical')
  checkArgClassValue(data.struct, 'character', expected.values = c('long', 'wide', 'longcombo'))
  checkArgClassValue(id.name, 'character')
  checkArgClassValue(time.name, 'character')
  checkArgClassValue(dat.name, 'character', expected.values = 'xCH4')
  checkArgClassValue(comp.name, c('character', 'NULL'))
  checkArgClassValue(cmethod, 'character', expected.values = c('removed', 'total'))
  # Skip imethod, checked in interp
  checkArgClassValue(extrap, 'logical')
  checkArgClassValue(addt0, 'logical')
  checkArgClassValue(showt0, 'logical')
  checkArgClassValue(dry, 'logical')
  checkArgClassValue(empty.name, c('character', 'NULL'))
  checkArgClassValue(std.message, 'logical')
  checkArgClassValue(check, 'logical')
  
  # Hard-wire rh for now at least
  if(!dry) {
    rh <- 1
  } else {
    rh <- 0
  }
  
  # Add check arguments from cumBg. Maybe it would make more sense to have one common function for data sorting and check arguments?? 
  
  # Add headspace if provided. NTS: This will be necessary for method 2, but not method 1. Figure whether to have both methods in one function.
  
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
  
  