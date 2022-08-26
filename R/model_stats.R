# Functions for model fit
# m = observed value, p = predicted value from model

me <- function(m, p) {
  tss <- sum((m - mean(m))^2)
  rss <- sum((m - p)^2)
  return(1 - rss/tss)
}

rmse <- function(m, p) {
  rss <- sum((m - p)^2)
  return(sqrt(rss/length(m)))
}

rmae <- function(m, p) {
    return(mean(abs((m - p)/m)))
}


mae <- function(m, p) {
    return(mean(abs(m - p)))
}

mbe <- function(m, p) {
    return(mean(p - m))
}

rs <- function(m,p) {
  return(m - p)
}