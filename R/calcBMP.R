calcBMP <- function(
  vol,
  setup,
  id.name = 'id',
  time.name = 'time',
  descrip.name = 'descrip',
  inoc.name = NULL,
  inoc.m.name = NULL,
  sub.name = NULL,
  sub.se.name = NULL,
  vol.name = 'cvCH4',
  check.val = FALSE,
  cell.crit = c(340, 395),
  rsd.crit = 0.06,
  dur.crit = '1p3d',
  cell.name = 'cell',
  out.name = 'BMP',
  out.append = Sys.time(),
  ...
  )
{

  cell.crit <- sort(cell.crit)

  # NTS: has to work with sets???
  # BMP
  res <- summBg(vol = vol,
                 setup = setup,
                 id.name = id.name,
                 time.name = time.name,
                 descrip.name = descrip.name,
                 inoc.name = inoc.name, 
                 inoc.m.name = inoc.m.name,
                 norm.name = sub.m.name,
                 norm.se.name = sub.se.name,
                 vol.name = vol.name,
	       	when = c('end', '1p3d', 'meas')
	       )

  res$rsd <- res$sd / res$mean
  BMP.end <- res[res$when == 'end', ]
  BMP.1p3d <- res[res$when == '1p3d', ]
  SMP <- res[res$when == 'meas', ]

  # Check validation criteria
  dur.val <- all(BMP.1p3d$rate.crit.met)
  BMP.cell <- BMP.end[BMP.end[, descrip.name] == cell.name, 'mean'] 
  rsd.cell <- BMP.end[BMP.end[, descrip.name] == cell.name, 'rsd'] 
  if (nrow(BMP.cell) > 1) stop('Acellt194')
  cell.val <- BMP.cell >= cell.crit[1] & BMP.cell <= cell.crit[2]
  rsd.val <- rsd.cell <= rsd.crit
  all.val <- dur.val & cell.val & rsd.val

  # Return results
  res <- list(BMP = BMP.end, BMP.


# 1. 1p3dnet BMP
# 2. end BMP
# 3. SMP curves
# 4. Rate info
# 5. Validation results
