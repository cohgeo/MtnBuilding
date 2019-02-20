# This script repeats creates erf(x) and erfc(x) functions and plots them.
# Updated 2019.01.23 CH.


## MAKE FUNCTIONS --------------------------------------------------------------

# Make function of the error function.
erf <- function(x) {
  erf <- 2 * pnorm(sqrt(2) * x) - 1
  return(erf)
}

# Make function of the complementary error function
erfc <- function(x) {
  erfc <- 2 * pnorm(-sqrt(2) * x)
  return(erfc)
}

## PLOT ------------------------------------------------------------------------

# Make a data frame of values to plot.
  # Make a sequence of x values.
  DF.erf <- data.frame("x" = seq(from = 0, to = 4, by = 0.1))
  # Calculate the error function.
  DF.erf$erf.x <- erf(DF.erf$x)
  # Calculate the complimentary function.
  DF.erf$erfc.x <- erfc(DF.erf$x)

# Plot.
  # Plot the error function.
  plot(x = DF.erf$x, 
       y = DF.erf$erf.x,
       type = "l",
       xlab = "x",
       ylab = "",
       col = "blue")
  # Prepare to add a new line to the plot.
  par(new = TRUE)
  # Plot the complimentary error function.
  plot(x = DF.erf$x, 
       y = DF.erf$erfc.x,
       type = "l",
       axes = FALSE,
       xlab = "", 
       ylab = "",
       col = "red")
  # Add labels for lines
  text(0.9, 0.9, 
       "erf(x)",
       col = "blue")
  text(0.9, 0.1, 
       "erfc(x)",
       col = "red")
  
  