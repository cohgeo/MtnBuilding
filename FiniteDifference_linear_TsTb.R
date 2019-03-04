# This script calculates a steady state geotherm based on a finite difference 
# model with constant heat flow with equations derived in class on 2/27/19 and 
# plots the geotherm.
# Updated 2019.02.27 CH.


## SETUP -----------------------------------------------------------------------

# Clear environment.
rm(list = ls())

# Load ggplot2.
library(ggplot2)

## INPUT AND CALCULATE PARAMETERS ----------------------------------------------

# Input constants and values. Convert values to the correct units for 
# calculations.

  # Input thermal conductivity in J/(s*K*m) or W/(m*K).
  k <- 3  # [W/(m*K)]

  # Input density in kg/(m^3).
  rho <- 2800  # [kg/(m^3)]

  # Input heat capacity in J/(kg*K).
  C <- 1e+03  # [J/(kg*K)]

  # Input heat production in W/(m^3).
  A <- 1e-6  # [W/(m^3)]

  # Input vertical velocity / uplift in mm/yr.
  U.mmyr <- 11  # [mm/yr]
  U <- U.mmyr / (31500000 * 1000)  # [m/s]
  
  # Input geothermal gradient in K/km.
  a.Kkm <- 20  # [K/km]
  a <- a.Kkm / 1000  # [K/m]

# Calculate parameters based on constants input above.

  # Calculate thermal diffusivity in (m^2)/s
  K <- k / (C * rho)  # [(m^2)/s]
  
  
## SET BOUNDARY CONDTIONS ------------------------------------------------------

# Input boundary condtions.
  
  # Input the surface temperature.
  Ts <- 20  # [°C]
  
  # Input the basal temperature.
  Tb <- 1020  # [°C]
  

## SET MODEL STEP SIZES --------------------------------------------------------
  
# Input the distance step for the model. Calculate the stable time step for the 
# model. Convert values to the correct units for calculations.
  
  # Input the change in distance.
  delta.x <- 1000  # [m]
  # delta.x2 <- delta.x ^ 2  # [m^2]
  
  # Calculate the change in time based on the stability criterion for the model.
  delta.t.s <- (delta.x ^ 2) / (2.9574 * K)  # [s]
  delta.t.Ma <- delta.t.s / 31560000000000  # [Ma]
  # K.delta.t <- K * delta.t.s   # [m^2]
  
  # Create a vector of distances over which to evaluate the model.
  max.depth.km <- 50  # Set maximum depth of model in km.
  x <- vector()  # Initialize vector.
  x <- seq(from = 0, to = (max.depth.km * 1000), by = delta.x)
  
  # Create a vector of time over which to evaluate the model.
  max.time.Ma <- 21  # Set maximum time of model in Ma.
  t.Ma <- vector()  # Initialize vector.
  t.Ma <- seq(from = 0, to = max.time.Ma, by = delta.t.Ma)
  t.s <- vector()  # Initialize vector.
  t.s <- seq(from = 0, to = (max.time.Ma * 31560000000000), by = delta.t.s)

  
## SOLVE FINITE DIFERENCE MODEL ------------------------------------------------
 

  
  # Initialize matrix to hold results of the model.
  TC <- matrix(data = NA, nrow = length(t.s), ncol = length(x))  
  
  # Populate first column of the matrix.
  TC[, 1] <- Ts
  
  # Populate first row of the matrix.
  for (i in 2:dim(TC)[2]) {
    TC[1, i] <- TC[1, 1] + a * x[i]
  }
  
  # Populate the last column of the matix.
  TC[, dim(TC)[2]] <- TC[1, dim(TC)[2]]
  
  # for (i in 2:dim(TC)[2] - 1) {
  #   for (j in 2:dim(TC)[1]) {
  #     TC[j, i] <- 3
  #   }
  # }
  
  
  # Populate the remaining rows of the matrix.
  for (i in 2:(dim(TC)[2] - 1)) {
    for (j in 2:dim(TC)[1]) {  
      TC[j, i] <- TC[j - 1, i] + 
        (K * delta.t.s) * (((TC[j - 1, i + 1] - 
                               (2 * TC[j - 1, i]) + 
                               TC[j - 1, i - 1]) / (delta.x ^ 2)) 
                           + (A / k) 
                           + ((U * (TC[j - 1, i + 1] - 
                                      TC[j - 1, i])) / (K * delta.x)))
    }
  }
  
  
  

  
  FiniteDifference.TsTb <- function(j, i) {
    TC[j, i] <- TC[j - 1, i] + 
      (K * delta.t.s) * (((TC[j - 1, i + 1] - 
                             (2 * TC[j - 1, i]) + 
                             TC[j - 1, i - 1]) / (delta.x ^ 2)) 
                         + (A / k) 
                         + ((U * (TC[j - 1, i + 1] - 
                                    TC[j - 1, i])) / (K * delta.x)))
  }
  
  TC[2,] <- mapply(FiniteDifference.TsTb, 2:dim(TC)[1], 2:(dim(TC)[2] - 1))
  
  TCtest <- mapply(FiniteDifference.TsTb, 2:107151, 2:50)
  
  
  
 #  mapply
  

  # TC[j, i] <- TC[j - 1, i] + 
  #   (K * delta.t.s) * (((TC[j - 1, i + 1] - 
  #                          (2 * TC[j - 1, i]) + 
  #                          TC[j - 1, i - 1]) / (delta.x ^ 2)) 
  #                      + (A / k) 
  #                      + ((U * (TC[j - 1, i + 1] - 
  #                                 TC[j - 1, i])) / (K * delta.x)))
  # 
  
  
  
  
  
  
  
  
  
  # TC[row, column] <- TC[row - 1, column] + 
  #   (K * delta.t.s) * (((TC[row - 1, column + 1] - 
  #                          (2 * TC[row - 1, column]) + 
  #                          TC[row - 1, column - 1]) / (delta.x ^ 2)) 
  #              + (A / k) 
  #              + ((U * (TC[row - 1, column + 1] - 
  #                         TC[row - 1, column])) / (K * delta.x)))
  
  
  
  
  
  

  