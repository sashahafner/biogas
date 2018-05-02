# Modified: 19 Aug 2016 SDH

watVap <- function(
  temp.k,     # Temperature in K
  pres.pa = NULL # Total pressure in Pa (optional)
  ) {

  # Saturated vapor pressure of water
  
  if(any(temp.k < 273.15 | temp.k > 373.15)) 
    warning('in low level function WatVap(), temp.k is ', temp.k, ' K. Is this really correct?')

  ## This is based on NIST parameter values from Gubkov, Fermor, et al. 1964: 6.20963 2354.731 7.559
  #                                                                            A      B         C
  ## Oirignal (NIST) was in bar and K, so A in Pa (only change) is therefore: 6.20963 + log10(1E5) = 6.20963 + 5 = 11.20963.
  #return(10^(11.20963 - 2354.731/(temp.k + 7.559)))

  # Magnus form equation from Alduchov and Eskridge J Applied Meteorology 35: 601-609
  # See Eq. (21) in this paper. 
  # Enhancement factor (total pressure dependent) is in Eq. (22).
  # Original units were hPa, therefore 100* was added
  temp.c <- temp.k - 273.15
  ew <- 100*6.1094*exp(17.625*temp.c/(243.04 + temp.c))

  # Add enhancement factor if total pressure is available
  if(!is.null(pres.pa) && is.numeric(pres.pa)) {
    ew <- 1.00071*exp(0.000000045*pres.pa)*ew
  }

  return(ew)

}

