# First-order model functions

FO1pCalc <- function(t, B, k, t.shift = 0, y.shift = 0) {

  y <- B * (1 - exp(-k * (t - t.shift))) + y.shift
  y[(t - t.shift) < 0] <- NA

  return(y)

  #if (fit.to == 'yield') return(y)

  ## Otherwise, return rates
  #r <- c(0, diff(y)/diff(t))
  #return(r)
}


FO1pObj <- function(x, t, y, fit.to = 'yield', t.shift = 0, y.shift = 0, SS = TRUE, fixed = NULL, fit.last = NULL, trans = FALSE, resids = FALSE) {

  # Parameter values come from fixed vector or else optimization algorithm
  if (!is.null(fixed)) {
    if ('B' %in% names(fixed)) {
      B <- fixed['B']
    } else {
      B <- x['B']
    }
    if ('k' %in% names(fixed)) {
      k <- fixed['k']
    } else {
      k <- x['k']
    }
  } else {
    B <- x['B']
    k <- x['k']
  }

  if (trans) {
    k <- 10^k
  }

  # If curve is forced through final point, calculate B from k
  if (fit.last) {
    tf <- max(t)
    B <- (y[t == tf] - y.shift) / (1 - exp(-k * (tf - t.shift)))
  }

  pred <- FO1pCalc(t = t, B = B, k = k, t.shift = t.shift, y.shift = y.shift)

  if (fit.to == 'rate') {
    pred <- diff(pred)/diff(t)
    y <- diff(y)/diff(t)
    t <- t[-1]
    rs <- pred - y
    rs[(t - t.shift) <= 0] <- 0
  } else {
    rs <- pred - y
    rs[(t - t.shift) < 0] <- 0
  }

  if (resids) return(rs)

  # Use sum of squares for objective as default
  # Values below t.shift have no weight
  if (SS) return(sum((rs^2)[t >= t.shift]))

  # Otherwise sum of absolute errors
  return(sum(abs(rs)[t >= t.shift]))
}




FO2pCalc <- function(t, B, f, k1, k2, t.shift = 0, y.shift = 0) {
  y <- B * (1 - f * exp(-k1 * (t - t.shift)) - (1 - f)  * exp(-k2 * (t - t.shift))) + y.shift
  y[(t - t.shift) < 0] <- NA

  return(y)

  #if (fit.to == 'yield') return(y)

  ## Otherwise, return rates
  #r <- c(0, diff(y)/diff(t))
  #return(r)
}




FO2pObj <- function(x, t, y, fit.to = 'yield', t.shift = 0, y.shift = 0, SS = TRUE, trans = FALSE, resids = FALSE) {

  B <- x['B']
  f <- x['f']
  k1 <- x['k1']
  k2 <- x['k2']

  if (trans) {
    f <- logistic(f)
    k1 <- 10^k1
    k2 <- 10^k2
  }

  pred <- FO2pCalc(t = t, B = B, f = f, k1 = k1, k2 = k2, t.shift = t.shift, y.shift = y.shift)

  if (fit.to == 'rate') {
    pred <- diff(pred)/diff(t)
    y <- diff(y)/diff(t)
    t <- t[-1]
    rs <- pred - y
    rs[(t - t.shift) <= 0] <- 0
  } else {
    rs <- pred - y
    rs[(t - t.shift) < 0] <- 0
  }
  
  if (resids) return(rs)

  # Use sum of squares for objective as default
  # Values below t.shift have no weight
  if (SS) return(sum((rs^2)[(t - t.shift) >= 0]))

  # Otherwise sum of absolute errors
  return(sum(abs(rs)[(t - t.shift) >= 0]))
}


fitFOM <- function(dat, n.pool = 1, 
                   time.name = 'time.d', resp.name = 'cvCH4',
                   fit.to = 'yield', method = 'Nelder-Mead', abs.err = FALSE, trans = TRUE,
                   init = if (n.pool == 1) c(B = 'yield', k = 0.5) else c(B = 'yield', f = 0.5, k1 = 0.01, k2 = 0.5), 
                   fixed = NULL, fit.last = FALSE, lower = NULL, upper = NULL,
                   lag.phase = FALSE) {

  t <- dat[, time.name]
  y <- dat[, resp.name]
  r <- c(0, diff(y)/diff(t))

  if (lag.phase) {
    l <- max(1, which.max(r) - 1)
    t.shift <- t[l]
    y.shift <- y[l]
  } else {
    l <- 0
    t.shift <- 0
    y.shift <- 0
  }

  # Adjust fixed B for lag production if Bu (not B) is fixed
  if (!is.null(fixed) & 'Bu' %in% names(fixed)) {
    fixed['B'] <- fixed['Bu'] - y.shift
    fixed <- fixed[names(fixed) != 'Bu']
  }

  # If initial B guess is to come from data, calculate it considering lag
  # Assume init must have a B element
  if (init['B'] == 'yield') {
    init['B'] <- 1.1 * max(dat[, resp.name]) - y.shift
    nn <- names(init)
    init <- as.numeric(init)
    names(init) <- nn
  }

  # Argument checks
  if (any(names(fixed) %in% names(init))) {
    warning('These parameters are given in both `init` and `fixed` arguments,\n and `init` values will be ignored: ',
             names(init)[names(init) %in% names(fixed)]) 
    init <- init[!names(init) %in% names(fixed)]
  }

  if (fit.last) {
    if ('B' %in% names(init)) {
      warning('When `fit.last = TRUE` the B parameter is linked \n directly to `k`.\n `init` entry for `B` will be ignored.')
      init <- init[names(init) != 'B']
    }
    if (!is.null(fixed) & 'B' %in% names(fixed)) {
      warning('When `fit.last = TRUE` the B parameter is linked \n directly to `k`.\n `fixed` entry for `B` will be ignored.')
      fixed <- fixed[names(fixed) != 'B']
      if (length(fixed) == 0) {
        fixed <- NULL
      }
    }
  }


  # Transform init etc.
  if (trans) {
    if (is.null(lower) | is.null(upper)) {
      if (n.pool == 1) {
        init['k'] <- log10(init['k'])
      } else {
        init['k1'] <- log10(init['k1'])
        init['k2'] <- log10(init['k2'])
        init['f'] <- logistic(init['f'])
      }
    } else {
      if (n.pool == 1) {
        lower['k'] <- log10(lower['k'])
        upper['k'] <- log10(upper['k'])
      } else {
        lower['k1'] <- log10(lower['k1'])
        lower['k2'] <- log10(lower['k2'])
        lower['f'] <- logistic(lower['f'])

        upper['k1'] <- log10(upper['k1'])
        upper['k2'] <- log10(upper['k2'])
        upper['f'] <- logistic(upper['f'])
      }
    }
  }

  if (method == 'LM') {

    if (!requireNamespace('minpack.lm')) {
      stop('You selected method = \'LM\' but the manpack.lm packge is not installed.\nPlease install minpack.lm in order to use this method.')
    }

    if (n.pool == 1) {

      mod <- minpack.lm::nls.lm(par = init, 
                    fn = FO1pObj, 
                    resids = TRUE,
                    t = t, y = y, t.shift = t.shift, y.shift = y.shift, SS = !abs.err,
                    fixed = fixed,
                    fit.last = fit.last,
                    fit.to = fit.to, 
                    trans = trans)

      coef.tab <- summary(mod)$coefficients

    } else if (n.pool == 2) {

      mod <- minpack.lm::nls.lm(par = init, 
                    fn = FO2pObj, 
                    resids = TRUE,
                    t = t, y = y, t.shift = t.shift, y.shift = y.shift, SS = !abs.err,
                    fit.to = fit.to, 
                    trans = trans)

      coef.tab <- summary(mod)$coefficients
 
    } else { # With parameter transformation
      stop('n.pool must be 1 or 2 but is ', n.pool, '.')
    }

    converge <- (mod$info == 1 || mod$info == 2 || mod$info == 3)
    converge.mssg <- mod$message

  } else if (method %in% c('Nelder-Mead', 'BFGS', 'CG', 'L-BFGS-B', 'SANN', 'Brent')) {

    if (is.null(lower)) lower <- -Inf 
    if (is.null(upper)) upper <- Inf

    if (n.pool == 1) {

      mod <- optim(par = init, fn = FO1pObj, resids = FALSE,
                   t = t, y = y, t.shift = t.shift, y.shift = y.shift, SS = !abs.err,
                   fixed = fixed,
                   fit.last = fit.last,
                   fit.to = fit.to, 
                   trans = trans,
                   method = method,
                   lower = lower,
                   upper = upper)
      coef.tab <- data.frame(Estimate = mod$par)

    } else if (n.pool == 2) {

      mod <- optim(par = init, fn = FO2pObj, resids = FALSE,
                   t = t, y = y, t.shift = t.shift, y.shift = y.shift, SS = !abs.err,
                   fit.to = fit.to, 
                   trans = trans,
                   method = method,
                   lower = lower,
                   upper = upper)
      coef.tab <- data.frame(Estimate = mod$par)

    } else {
      stop('n.pool must be 1 or 2 but is ', n.pool, '.')
    }

    converge <- mod$convergence == 0
    converge.mssg <- mod$convergence

  }

  # Add fixed pars to coef.tab
  if (!is.null(fixed)) {
    fc <- matrix(c(fixed, rep(rep(NA, 3), length(fixed))), nrow = length(fixed), dimnames = list(names(fixed), colnames(coef.tab)))
    coef.tab <- rbind(coef.tab, fc)
  }

  # Add B calculated from k if fit.last = TRUE
  if (fit.last) {
    tf <- max(t)
    k <- coef.tab['k', 1]
    if (trans) k <- 10^k
    B <- (y[t == tf] - y.shift) / (1 - exp(-k * (tf - t.shift)))
    fc <- matrix(c(B, rep(NA, 3)), nrow = 1, dimnames = list('B', colnames(coef.tab)))
    coef.tab <- rbind(coef.tab, fc)
  }

  # Model predictions
  if (n.pool == 1) {
    B <- coef.tab['B', 1]
    B.se <- coef.tab['B', 2]
    k <- coef.tab['k', 1]
    k.se <- coef.tab['k', 2]
    
    if (trans) k <- 10^k
    coefs <- c(B = B, k = k, B.se = B.se, k.se = k.se)
    preds <- data.frame(t = t, y = FO1pCalc(t = t, B = B, k = k,
                                            t.shift = t.shift, y.shift = y.shift))
  } else {

    B <- coef.tab['B', 1]
    B.se <- coef.tab['B', 2]
    f <- coef.tab['f', 1]
    f.se <- coef.tab['f', 2]
    k1 <- coef.tab['k1', 1]
    k1.se <- coef.tab['k1', 2]
    k2 <- coef.tab['k2', 1]
    k2.se <- coef.tab['k2', 2]

    if (trans) {
      f <- logistic(f)
      k1 <- 10^k1
      k2 <- 10^k2
    }

    if (k1 < k2) {
      f <- 1 - f
      k1t <- k1
      k1.set <- k1.se
      k1 <- k2
      k1.se <- k2.se
      k2 <- k1t
      k2.se <- k1.set
    }

    coefs <- c(B = B, f = f, k1 = k1, k2 = k2, B.se = B.se, f.se = f.se, k1.se = k1.se, k2.se = k2.se)
    preds <- data.frame(t = t, y = FO2pCalc(t = t, B = B, f = f, k1 = k1, k2 = k2,
                                            t.shift = t.shift, y.shift = y.shift))
  }

  i.eval <- (t - t.shift) >= 0

  # Get n and max rate drop (sampling intensity)
  nn <- sum(!is.na(dat[, resp.name]))

  if (fit.to == 'rate') {
    preds$r <- c(NA, diff(preds$y)/diff(preds$t))
    pred <- preds[, 3]
    resid <- pred - r
    names(preds) <- c(time.name, resp.name, 'rate')
    ME <- me(r[i.eval][-1], pred[i.eval][-1])
    RMSE <- rmse(r[i.eval][-1], pred[i.eval][-1])
    MAE <- mae(r[i.eval][-1], pred[i.eval][-1])
    fit <- c(ME = ME, RMSE = RMSE, MAE = MAE)
    nn <- nn - 1
    r <-  diff(dat[, resp.name])/diff(dat[, time.name])
    dr <- diff(r)
    max.delta <- min(c(0, dr)) / max(r)
  } else {
    preds$r <- c(NA, diff(preds$y)/diff(preds$t))
    pred <- preds[, 2]
    resid <- pred - y
    names(preds) <- c(time.name, resp.name, 'rate')
    ME <- me(y[i.eval], pred[i.eval])
    RMSE <- rmse(y[i.eval], pred[i.eval])
    MAE <- mae(y[i.eval], pred[i.eval])
    fit <- c(ME = ME, RMSE = RMSE, MAE = MAE)
    dy <- diff(dat[, resp.name])
    max.delta <- max(c(0, dy)) / max(dat[, resp.name])
  }

  # max.delta is maximum change in either yield or rate as a fraction of maximum value

  return(list(model = mod, ilag = l, tlag = t.shift, ylag = y.shift, 
              n = nn, max.delta = max.delta,
              coefs = coefs, coef.tab = coef.tab, preds = preds, 
              pred = pred, fit = fit, resid = resid,
              converge = converge, converge.mssg = converge.mssg,
              summ = data.frame(ilag = l, tlag = t.shift, ylag = y.shift, 
                                n = nn, max.delta = max.delta,
                                t(coefs), t(fit), converge = converge, converge.mssg = converge.mssg)
             )
       )

}
