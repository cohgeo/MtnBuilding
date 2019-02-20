# This script repeats creates a function for calculating the error function,
# erf(x).
# Updated 2019.02.20 CH.

erf <- function(x) {
  erf <- 2 * pnorm(sqrt(2) * x) - 1
  return(erf)
}