# This script calculates a steady state geotherm based on equations derived in
# class on 1/31/19 and plots the geotherm utilizing multiple layers with 
# changing A with depth.
# Updated 2019.02.08 CH.


## SETUP -----------------------------------------------------------------------

# Clear environment.
rm(list = ls())

# Load ggplot2.
library(ggplot2)

## INPUT CONSTANTS AND VALUES --------------------------------------------------

# Input constants and values.
# Convert values to the correct units for calculations.

# Set number of layers for model.
n = 4

# Input the thickness of each layer.
# D[i] = thickness of layer i.
  # Initialize vector of legth n with dummy thicknesses of 10 km.
  # D.km <- rep(10, n)  # [km]
  # Set thicknesses of layers in km.
  D.km <- c(5, 15, 25, 5)  # [km]
  # Convert layer thickness to m.
  D <- D.km * 1000  # [m]

# Input the heat production in each layer.
# A[i] = heat production in layer i.
  # Input heat production in μW/m^3.
  A.uW <- c(8, 1, 2, 2.5)  # [μW/m^3]
  # CConvert heat production to W/m^3.
  A <- A.uW * 10^-6  # [W/m^3]

# Input the thermal conductivity in each layer.
# k[i] = thermal conductivity in layer i.
  # Input thermal conductivity in J/(s*K*m) or W/(m*K).
  k <- c(3, 3, 3, 3)  # [W/(m*K)]

# Input the heat flow (Qb = Q* = Qr) at the base of the model.
  # Input basal heat flow in mW/m^2.
  Qb.mW <- 18  # [mW/m^2]
  # Convert basal heat flow to W/m^2.
  Qb <- Qb.mW / 1000  # [W/m^2]

# Input the depth at D0 (should always be 0 km and 0 m).
  D0 <- 0
  
# Set the surface temperture of the model.
  T0.C <- 25  # temperature at the surface of layer 1 [°C]
  T0 <- T0.C + 273.15  # temperature at the surface of layer 1 [K]

  
## MARK'S RESULTS --------------------------------------------------------------
  
  # Q* = 0.018 W/m2
  # QDi = c(3.8e-2, 2.10e-2, 0.018)
  # QDi = c(0.038, 0.021, 0.018)
  # TDi = c(121, 264, 435)
  
## CALCULATIONS ----------------------------------------------------------------

# Calculate Q[i], the heat flow into the base of layer i.
  # Make vector of A[i] * D[i].
  AD <- A * D
  # Initialize sumAD vector.
  sumAD <- NULL
  # Initialize Q vector.
  Q <- NULL
  # Where j = i + 1, calculate Q[i].
  for(i in 1:(n - 1)){
    # Sum AD vector from j to n.
    sumAD[i] <- sum(AD[(i + 1):n])
    # Add Qb to the sum of the AD vector from j to n.
    Q[[i]] <- Qb + sumAD[i]
  }
  # Add Qn = Qb to the vector of Q values.
  Q[[n]] <- Qb
  
# Calculate TD[i], the temperature at the boundary/base of layer i, which will
# become the temperature at the top of layer i + 1.
  # Initialize TD vector.
  TD <- NULL
  # Calculate TD[1]. This needs to be calculated separately from i > 1 because
  # TD[1-1] = TD[0] is not an indexable position on the TD vector but it is
  # equal to T0, the temperature at the surface of the model.
  TD[[1]] <- T0 + 
    ((Q[1] * D[1]) / k[1]) +
    ((A[1] * (D[1] ^ 2)) / (2 * k[1]))  # [K]
  # Calculate TD for i values > 1.
  for (i in 2:n) {
    TD[[i]] <- TD[i - 1] + 
      ((Q[i] * D[i]) / k[i]) +
      ((A[i] * (D[i] ^ 2)) / (2 * k[i]))  # [K]
  }

# Create a vector of temperatures in K to use at as the surface of each layer.
  # Initialize vector Ts, the temperature at the surface of each layer.
  Ts <- NULL
  Ts[[1]] <- T0
  for (i in 1:n + 1) {
    Ts[[i]] <- TD[i - 1]
  }

# Calculate Tl[i], the temperature within a layer.
  # Create a list of z values for each layer.
  z <- list()  # Preallocate z.
  for (i in 1:n) { 
    z[[i]] <- seq(from = 0, to = D[i], by = 100)
    names(z)[i] <- paste("z", i, sep = "")
  }
  # Preallocate Tl[i].
  Tl <- list()
  # Calculate Tl[i].
  for (i in 1:n) {
    Tl[[i]] <- sapply(z[i], function(x) Ts[i] +
                   ((Q[i] + (A[i] * D[i])) / k[i]) * x -
                   ((A[i] * (x ^ 2)) / (2 * k[i])))
    names(Tl)[i] <- paste("Tl", i, sep = "")
  }
  
## ADD A MANTLE LAYER BELOW THE CRUST LAYERS -----------------------------------

# Set mantle properties.
  A.mantle <- 0  # [μW/m^3]
  A.mantle <- A.mantle * 10^-6  # [W/m^3]
  k.mantle <- 3  # [W/m^2]
  D.mantle <- 100  # [km]
  D.mantle <- D.mantle * 1000  # [m]
  Q.mantle <- Qb  # This does not effect the solution when A.mantle = 0.

# Calculate Tl[n + 1], the temperature within the mantle layer.
  # Create a list of z values for each layer.
  for (i in n + 1) {
    z[[i]] <- seq(from = 0, to = D.mantle, by = 100)
    names(z)[i] <- paste("z", i, sep = "")
  }
  # Calculate Tl[mantle].
    Tl[[n + 1]] <- sapply(z[i], function(x) TD[n] +
                        ((Q.mantle + (A.mantle * D.mantle)) / k.mantle) * x -
                        ((A.mantle * (x ^ 2)) / (2 * k.mantle)))
    names(Tl)[n + 1] <- paste("Tl.mantle")
  

## SETUP FOR PLOTTING ----------------------------------------------------------

# Combine solutions into one data frame.
  # Preallocate z.true, a vector of depth from the surface of the model in m.
  z.true <- list()
  # Calculate z.true, the depth of each z value from the surface of the model
  # for layers > 1.
    # Add mantle depth to D vector.
    D[[n + 1]] <- D.mantle
    # Preallocate sumD, the true depth at the top of each layer.
    sumD <- NULL
    # Save the surface depth as sumD at position 1.
    sumD[[1]] <- D0
    # Sum the D vector from 1 to i - 1 to calculate the true starting depth of
    # each layer from 2 to n.
    for (i in 1:n + 1) {
      sumD[[i]] <- sum(D[1:(i - 1)])
    }
  # Calculated the true depth for each z value for layers 2 to n.
  for (i in 1:n + 1) {
    z.true[[i]] <- sapply(z[i], function(x) x + sumD[i])
  }
  # Save the values of z for layer 1 in the z.true list.
  z.true[[1]] <- z[[1]]
  # Create a data frame of true depths and temperature values.
  DF <- data.frame("z.true.m" = unlist(z.true),
                   "z.m" = unlist(z),
                   "Tl.K" = unlist(Tl))
  # Convert z.true.m to km for plotting.
  DF$z.true.km <- DF$z.true.m / 1000
  # Convert Tl from K to C for plotting.
  DF$Tl.C <- DF$Tl.K - 273.15
  

## PLOT THE GEOTHERM FOR n LAYERS ----------------------------------------------
    
# PLOT T vs. z:

  ggplot() +
    # Make a dashed horizontal line showing the depth of D1.
    geom_line(aes(x = seq(from = 0, to =  max(DF$Tl.C), by = 100),
                  y = sumD[2] / 1000),
              size = 0.75,
              color = "gray",
              linetype = 2) +
    # Label D1 line.
    geom_text(aes(x = max(DF$Tl.C),
                  y = sumD[2] / 1000,
                  label = "D1")) +
    # Make a dashed horizontal line showing the depth of D2.
    geom_line(aes(x = seq(from = 0, to =  max(DF$Tl.C), by = 100),
                  y = sumD[3] / 1000),
              size = 0.75,
              color = "gray",
              linetype = 2) +
    # Label D2 line.
    geom_text(aes(x = max(DF$Tl.C),
                  y = sumD[3] / 1000,
                  label = "D2")) +
    # Make a dashed horizontal line showing the depth of D3.
    geom_line(aes(x = seq(from = 0, to =  max(DF$Tl.C), by = 100),
                  y = sumD[4] / 1000),
              size = 0.75,
              color = "gray",
              linetype = 2) +
    # Label D3 line.
    geom_text(aes(x = max(DF$Tl.C),
                  y = sumD[4] / 1000,
                  label = "D3")) +
    # Make a dashed horizontal line showing the depth of D4.
    geom_line(aes(x = seq(from = 0, to =  max(DF$Tl.C), by = 100),
                  y = sumD[5] / 1000),
              size = 0.75,
              color = "gray",
              linetype = 2) +
    # Label D3 line.
    geom_text(aes(x = max(DF$Tl.C),
                  y = sumD[5] / 1000,
                  label = "MOHO")) +
  
    # Plot Tl.C vs. z.true.km.
    geom_path(data = DF,
              aes(x = Tl.C,
                  y = z.true.km),
              size = 1,
              color = "tomato2") +
    # Make y axis plot in reverse order (0 depth at top of plot).
    scale_y_reverse() +
    # Change to simpler ggplot2 theme.
    theme_bw() +
    # # Change legend title.
    # guides(color = guide_legend(title = "Boundary Conditions")) +
    # Title plot and label axes.
    labs(title = "Steady state geotherm: multilayer model (n = 4)",  # Title plot.
         x = "Temperature (°C)",  # Label x axis.
         y = "Depth (km)")  # Label y axis.
