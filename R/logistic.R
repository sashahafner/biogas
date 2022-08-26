# Logistic and logit functions

logistic <- function(x) exp(x)/(1 + exp(x))
logit <- function(p) log(p/(1 - p))

