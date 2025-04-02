# Fermentation stoichiometry
# Example calls:
# source('readFormula.R')
# predFerm('C6H10O5', acefrac = 0, fs = 0.1)
# predFerm('C6H10O5', acefrac = 0.5, fs = 0.1)
# predFerm('C6H10O5', acefrac = 1)
# predFerm('C6H10O5', acefrac = 1)

# Function to get stoichiometry for custom organic reaction (O-19)
customOrgStoich <- function(form, elements =  c('C', 'H', 'O', 'N')) {
  
  fc <- readFormula(form, elements)

  # Use symbols from O-19 in R&M
  n <- as.numeric(fc['C'])
  a <- as.numeric(fc['H'])
  b <- as.numeric(fc['O'])
  cc <- as.numeric(fc['N'])
  d <- 4 * n + a - 2 * b - 3 * cc
  
  # Put together
  rr <- c(CO2 = - (n - cc) / d, NH4. = - cc / d, HCO3. = - cc / d)
  rr[form] <-  1/d
  
  return(rr)
  
}

predFerm <- function(
  subform = NULL,           # Character chemical formula of substrate
  biomassform = 'C5H7O2N',  # Biomass empirical formula
  acefrac = 0.5,            # Acetate (vs. H2) fraction
  fs = 0,                    # Fraction substrate going to cell synthesis, fs in Rittmann and McCarty
  elements = c('C', 'H', 'O', 'N')
  ) {

  # Donor half reaction
  rd <- customOrgStoich(subform, elements = elements)

  # Synthesis half reaction
  rc <- customOrgStoich(biomassform, elements = elements)

  # Acceptor reactions
  # Acetate production
  raa <- c(CO2 = - 1/8, HCO3. = - 1/8, CH3COO. = 1/8, H2O = 3/8)
  # Hydrogen production
  rah <- c(H. = - 1, H2 = 1 / 2)

  ii <- unique(names(c(rd, rc, raa, rah)))

  # Blanks
  rd[ii[!ii %in% names(rd)]] <- 0
  rc[ii[!ii %in% names(rc)]] <- 0
  raa[ii[!ii %in% names(raa)]] <- 0
  rah[ii[!ii %in% names(rah)]] <- 0

  # Order
  rd <- rd[ii]
  rc <- rc[ii]
  raa <- raa[ii]
  rah <- rah[ii]

  # Acceptor reaction
  ra <- acefrac * raa + (1 - acefrac) * rah - rd

  fe <- 1 - fs
  
  # Combine
  rtot <- fe * ra + fs * rc  - rd

  return(rtot)

}
