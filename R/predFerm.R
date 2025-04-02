# Fermentation

source('readFormula.R')
predFerm('C6H10O5', ace.frac = 0)
predFerm('C6H10O5', ace.frac = 0.5)
predFerm('C6H10O5', ace.frac = 1)
predFerm('C6H10O5', ace.frac = 1)

predFerm <- function(
  form = NULL,            # Character chemical formula of substrate
  ace.frac = 0.5,         # Acetate (vs. H2) fraction
  fs = 0                  # Fraction substrate going to cell synthesis, fs in Rittmann and McCarty
  ) {

  fc <- readFormula(form, elements = c('C', 'H', 'O', 'N'))

  # Use symbols from O-19 in R&M
  n <- as.numeric(fc['C'])
  a <- as.numeric(fc['H'])
  b <- as.numeric(fc['O'])
  cc <- as.numeric(fc['N'])
  d <- 4 * n + a - 2 * b - 3 * cc

  rd <- c(CO2 = - (n - cc) / d, NH4. = - cc / d, HCO3. = - cc / d)
  rd[form] <-  1/d

  raa <- c(CO2 = - 1/8, HCO3. = - 1/8, CH3COO. = 1/8, H2O = 3/8)
  rah <- c(H. = - 1, H2 = 1 / 2)

  ii <- unique(names(c(rd, raa, rah)))

  # Blanks
  rd[ii[!ii %in% names(rd)]] <- 0
  raa[ii[!ii %in% names(raa)]] <- 0
  rah[ii[!ii %in% names(rah)]] <- 0

  # Order
  rd <- rd[ii]
  raa <- raa[ii]
  rah <- rah[ii]

  # Combine
  rtot <- ace.frac * raa + (1 - ace.frac) * rah - rd

  return(rtot)

}
