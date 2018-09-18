
planBMP <- function(
  vs.inoc,
  vs.sub,
  isr = NA,
  m.inoc = NA,
  m.sub = NA,
  m.tot = NA,
  m.vs.sub = vs.sub*m.sub,
  digits = 3,
  warn = TRUE,
  nice = TRUE
  ) {

  # Given VS concentrations, inoc mass, and ISR
  if(all(!is.na(vs.inoc + vs.sub + m.inoc + isr))) {

      m.tot <- m.inoc*(1 + vs.inoc/(isr*vs.sub))
      m.sub <- m.inoc*vs.inoc/(isr*vs.sub)
      m.vs.sub <- vs.sub*m.sub

  # Given VS concentrations, total mass, and ISR
  } else if(all(!is.na(vs.inoc + vs.sub + m.tot + isr))) {

      m.inoc <- m.tot/(1 + vs.inoc/(isr*vs.sub))
      m.sub <- m.inoc*vs.inoc/(isr*vs.sub)
      m.vs.sub <- vs.sub*m.sub

  # Given VS concentrations, substrate VS mass, and ISR
  } else if(all(!is.na(vs.inoc + vs.sub + isr + m.vs.sub))) {

      m.sub <- m.vs.sub/vs.sub
      m.inoc <- isr*m.sub*vs.sub/vs.inoc
      m.tot <- m.sub + m.inoc

  # Given VS concentrations, substrate mass, and inoculum mass (calculate ISR here)
  } else if(all(!is.na(vs.inoc + vs.sub + m.sub + m.inoc))) {

      isr <- vs.inoc*m.inoc/(vs.sub*m.sub)
      m.vs.sub <- vs.sub*m.sub
      m.tot <- m.inoc + m.sub

  } else {

      stop('Not enough input arguments. You must provide:
           vs.inoc, vs.sub, isr, and m.tot OR
           vs.inoc, vs.sub, isr, and m.sub OR
           vs.inoc, vs.sub, isr, and m.inoc OR
           vs.inoc, vs.sub, isr, and m.vs.sub OR
           vs.inoc, vs.sub, m.inoc, and m.sub'
           )

  }

  # Get inoculum VS mass, total VS mass, and total VS concentration
  m.vs.inoc <- vs.inoc*m.inoc
  m.vs.tot <- m.vs.sub + m.vs.inoc
  vs.mix <- m.vs.tot/m.tot
  
  if(warn) {
    if(any(vs.inoc > 1, vs.sub > 1)) warning('One or more VS concentration is > 1 g/g (1000 g/kg).')
    if(any(isr < 2)) warning('Inoculum-to-substrate ratio (isr argument) is < 2.')
    if(any(isr > 4)) warning('Inoculum-to-substrate ratio (isr argument) is > 4.')
    if(any(vs.mix < 0.02) | any(vs.mix > 0.06)) warning('Mixture VS concentration is not within 0.02 - 0.06 g/g (20 - 60 g/kg).')
    if(any(m.vs.sub < 2)) warning('Substrate VS mass is < 2 g.')
  }

  # Make a matrix of results (needed in case call is vectorized)
  res <- cbind(vs.inoc = vs.inoc, vs.sub = vs.sub, vs.mix = vs.mix,
	   isr = isr, 
	   m.inoc = m.inoc, m.sub = m.sub, m.tot = m.tot,
	   m.vs.sub = m.vs.sub, m.vs.inoc = m.vs.inoc, m.vs.tot = m.vs.tot
	   )

  # Turn into a vector or data frame
  if(nrow(res) == 1) {
      res <- res[1, ]
  } else {
      res <- as.data.frame(res)
  }

  res <- signif(res, digits)

  # Reformat only if form is numeric vector
  if(is.numeric(res) & nice) {
    nms <- c('Inoc. VS conc.', 'Substrate VS conc.', 'Mix. VS conc.',
             'ISR (VS basis)', 
             'Inoc. mass', 'Substrate mass', 'Mixture mass', 
             'Sub. VS mass', 'Inoc. VS mass', 'Mix. VS mass')
    
    unts <- c('g/g', 'g/g', 'g/g',
             'g/g', 'g', 'g', 'g', 
             'g', 'g', 'g')


    tab <- cbind(Parameter = c('---------', nms), Unit = c('----', unts), Value = c('-----', res))
    rownames(tab) <- NULL

    prmatrix(tab, quote = FALSE, rowlab = rep('', nrow(tab)))

    return(invisible(res))
  } else {
    return(res)
  }

}
