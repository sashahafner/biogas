# Modified: 10 Mar 2017

calcCOD <-
function(form) {

  # Check argument
  checkArgClassValue(form, 'character')

  # Get molar masses
  mmass <- molMass(form)

  # Read chemical formula, calculate COD
  fc <- list()
  COD <- numeric()
  for(i in 1:length(form)) {
    # If and only if first letter of form is lowercase, entire string is capitalized
    if(grepl('^[a-z]', form[i])) form[i] <- toupper(form[i])
    # Read formula (function not vectorized)
    fc[[i]] <- readFormula(form[i], elements = c('C', 'H', 'O', 'N'))
    # Calculate COD based on Rittmann and McCarty
    COD[i] <- as.vector((2*fc[[i]]['C'] + 0.5*fc[[i]]['H'] - 1.5*fc[[i]]['N'] - fc[[i]]['O'])*molMass('O')/mmass[i])
  }

  return(COD)
}
