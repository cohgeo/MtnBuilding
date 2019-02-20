# This script repeats creates a function for calculating the complimentary 
# error function, erfc(x).
# Updated 2019.02.20 CH.

erfc <- function(x) {
  erfc <- 2 * pnorm(-sqrt(2) * x)
  return(erfc)
}