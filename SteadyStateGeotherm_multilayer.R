# This script calculates a steady state geotherm based on equations derived in
# class on 1/24/19 and plots the geotherm utilizing multiple layers.
# Updated 2019.01.29 CH.


## SETUP -----------------------------------------------------------------------

# Load ggplot2.
library(ggplot2)


## INPUT CONSTANTS AND VALUES --------------------------------------------------

# Input constants and values.
# Convert values to the correct units for calculations.

# Common inputs:
# Oceanic surface heat flow ranges from 59 to 95 mW/m^2.
# Continental surface heat flow ranges from 38 to 200 mW/m^2.
# Thermal conductivity.
# Reasonable values: k is between 1.5 and 3.0 J/(s*m*K).
# Heat production.
# "Heat production measurements from 31 exposed granulite terranes range from 
# 0.1 to 2.7 μW m−3, with a mean of 0.68 ± 0.62 μW m−3 and a median of 0.45 
# μW m−3."  (Furlong and Chapman, 2013)
# "Average heat production from compilations of continental lithospheric 
# peridotites varies from 0.006 μW m−3 in exposed off-craton massifs to 
# 0.044 μW m−3 in cratonic xenoliths (Rudnick et al. 1998)" 
# (Furlong and Chapman, 2013)
# Felsic rock heat production: 1 to 4 μW/m^3
# Median upper crust heat production: 1.50 μW/m^3
# Median lower crust heat production: 1.45 μW/m^3


# LAYER 1: continental crust
D1.km = 41              # depth of layer 1 relative to the surface [km]
  D1 = D1.km * 1000     # depth of layer 1 relative to the surface [m]
Ts1.C = 20              # temperature at the surface of layer 1 [°C]
  Ts1 = Ts1.C + 273.15  # temperature at the surface of layer 1 [K]
Tb1.C = 1300            # basal temperature of layer 1 [°C]
  Tb1 = Tb1.C + 273.15  # basal temperature of layer 1 [K]
Qs1.mW = 41             # surface heat flow of layer 1 [mW/m^2]
  Qs1 = Qs1.mW / 1000   # surface heat flow of layer 1 [W/m^2]
Qb1.mW = 10             # basal heat flow of layer 1 [mW/m^2]
  Qb1 = Qb1.mW / 1000   # basal heat flow of lyaer 1 [W/m^2]
A1.uW = 0.4             # heat production in layer 1 [μW/m^3]
  A1 = A1.uW * 10^-6    # heat production in layer 1 [W/m^3]
k1 = 3                  # thermal conductivity of layer 1 [J/(s*K*m) or W/(m*K)]

# LAYER 2: lithospheric mantle
D2.km = 200             # depth of layer 2 relative to the surface [km]
  D2 = D2.km * 1000     # depth of layer 2 relative to the surface [m]
Ts2.C = Tb1.C           # T at the surface of layer 2 (Tb of layer 1) [°C]
  Ts2 = Ts2.C + 273.15  # T at the surface of layer 2 (Tb of layer 1) [K]
Tb2.C = 1600            # basal temperature of layer 2 [°C]
  Tb2 = Tb2.C + 273.15  # basal temperature of layer 2 [K]
Qs2.mW = Qb1.mW         # surface heat flow of layer 2 (Qb of layer 1) [mW/m^2]
  Qs2 = Qs2.mW / 1000   # surface heat flow of layer 2 (Qb of layer 1) [W/m^2]
Qb2.mW = 60             # basal heat flow of layer 2 [mW/m^2]
  Qb2 = Qb2.mW / 1000   # basal heat flow of lyaer 3 [W/m^2]
A2.uW = 0.019           # heat production in layer 2 [μW/m^3]
  A2 = A2.uW * 10^-6    # heat production in layer 2 [W/m^3]
k2 = 3                  # thermal conductivity of layer 2 [J/(s*K*m) or W/(m*K)]


## CALCULATIONS AND PLOT 1: Boundary conditions Ts, Tb -------------------------

# Boundary condtions:
# 1. T = T.s at z = 0 (constant surface temperature)
# 2. T = T.b at z = D (constant basal temperature)

# Assumptions:
# 1. U.z = 0  No advection.
# 2. A is constant and set above.
# 3. See variables and assigned values above.


# CALCULATE Tz:

# For 0 ≤ z < D, calculate temperature for a given z value (Tz).

  # Create a data frame with a column of z values from 0 to D1 in m.
  DF.BC1A <- data.frame("z" = seq(from = 0, to = D1, by = 1000))

  # Calculate Tz for layer 1.
  DF.BC1A$Tz <- Ts1 +
    (((Tb1 - Ts1) + ((A1 * (D1 ^ 2)) / (2 * k1))) / D1) * DF.BC1A$z -
    ((A1 * (DF.BC1A$z ^ 2)) / (2 * k1))

# For z > D, calculate temperature for a given z value (Tz).
  
  # Create a data frame with a column of z values from D1 to D2 in m.
  DF.BC1B <- data.frame("z" = seq(from = D1, to = D2, by = 1000))

  # Calculate Tz for layer 2.
  DF.BC1B$Tz <- Tb2 +
    (DF.BC1B$z - D2) * ((((Tb2 - Ts2) + ((A2 * (D2 ^ 2)) / (2 * k2))) / D2) - 
                        ((A2 * D2) / k2))

  # Bind data frames vertically to prepare for plotting.
  DF.BC1 <- rbind(DF.BC1A, DF.BC1B)
  # Remove extra data frames.
  rm(DF.BC1A, DF.BC1B)


# # PLOT T vs. z:
# 
# ggplot() +
#   # Plot Tz vs. z for boundary conditions 1.
#   geom_path(data = DF.BC1,
#             aes(x = Tz - 273.15,
#                 y = z / 1000),
#             color = "darkblue",
#             size = 1) +
#   # Make y axis plot in reverse order (0 depth at top of plot).
#   scale_y_reverse() +
#   # Change to simpler ggplot2 theme.
#   theme_bw() +
#   labs(title = "Steady state geotherm (Boundary condtions: constant Ts, constant Tb)",  # Title plot.
#        x = "Temperature (°C)",  # Label x axis.
#        y = "Depth (km)")  # Label y axis.


## CALCULATIONS AND PLOT 2: Boundary conditions Ts, Qs -------------------------

# Boundary condtions:
# 1. T = T.s at z = 0 (constant surface temperature)
# 2. Q = Q.s at z = D (constant surface heat flow)

# Assumptions:
# 1. U.z = 0  No advection.
# 2. A is constant and set above.
# 3. See variables and assigned values above.


# CALCULATE Tz:

# For 0 ≤ z < D, calculate temperature for a given z value (Tz).

  # Create a data frame with a column of z values from 0 to D1 in m.
  DF.BC2A <- data.frame("z" = seq(from = 0, to = D1, by = 1000))

  # Calculate Tz for layer 1.
  DF.BC2A$Tz <- Ts1 +
    ((Qs1 / k1) * DF.BC2A$z) -
    ((A1 * (DF.BC2A$z ^ 2)) / (2 * k1))
  
# For z > D, calculate temperature for a given z value (Tz).
  
  # Create a data frame with a column of z values from D1 to D2 in m.
  DF.BC2B <- data.frame("z" = seq(from = D1, to = D2, by = 1000))

  # Calculate Tz for layer 2.
  DF.BC2B$Tz <- Ts2 +
    (((Qs2 * D2) / k2) - ((A2 * (D2 ^ 2)) / (2 * k2))) +
    ((Qs2 - (A2 * D2)) / k2) * (DF.BC2B$z - D2)
  
# Bind data frames vertically to prepare for plotting.
  DF.BC2 <- rbind(DF.BC2A, DF.BC2B)
  # Remove extra data frames.
  rm(DF.BC2A, DF.BC2B)
  
  
# # PLOT T vs. z:
#   
#   ggplot() +
#     # Plot Tz vs. z for boundary conditions 2.
#     geom_path(data = DF.BC2,
#               aes(x = Tz - 273.15,
#                   y = z / 1000),
#               color = "cadetblue3",
#               size = 1) +
#     # Make y axis plot in reverse order (0 depth at top of plot).
#     scale_y_reverse() +
#     # Change to simpler ggplot2 theme.
#     theme_bw() +
#     labs(title = "Steady state geotherm (Boundary condtions: constant Ts, constant Qs)",  # Title plot.
#          x = "Temperature (°C)",  # Label x axis.
#          y = "Depth (km)")  # Label y axis.
  

## PLOT BOTH GEOTHERMS ---------------------------------------------------------

# Combine solutions into one data frame.
  DF.BC1$BC <- "TsTb"
  DF.BC2$BC <- "TsQs"
  DF <- rbind(DF.BC1, DF.BC2)
  
# PLOT T vs. z:
  
  ggplot() +
    # Make a dashed horizontal line showing the depth of D1.
    geom_line(aes(x = seq(from = 0, to =  max(DF$Tz), by = 100),
                  y = D1.km),
              size = 1,
              color = "gray",
              linetype = 2) +
    # Label D1 line.
    geom_text(aes(x = max(DF$Tz), 
                  y = D1.km, 
                  label = "D1")) +
    # Make a dashed horizontal line showing the depth of D1.
    geom_line(aes(x = seq(from = 0, to =  max(DF$Tz), by = 100),
                  y = D2.km),
              size = 1,
              color = "gray",
              linetype = 2) +
    # Label D2 line.
    geom_text(aes(x = max(DF$Tz), 
                  y = D2.km, 
                  label = "D2")) +
    # Plot Tz vs. z for boundary conditions 1, color by boundary condtions.
    geom_path(data = DF,
              aes(x = Tz - 273.15,
                  y = z / 1000,
                  color = BC),
              size = 1) +
    # Make y axis plot in reverse order (0 depth at top of plot).
    scale_y_reverse() +
    # Change to simpler ggplot2 theme.
    theme_bw() +
    # Change legend title.
    guides(color = guide_legend(title = "Boundary Conditions")) +
    # Title plot and label axes.
    labs(title = "Steady state geotherm",  # Title plot.
         x = "Temperature (°C)",  # Label x axis.
         y = "Depth (km)")  # Label y axis.
  