# This script calculates a steady state geotherm based on equations derived in
# class on 1/24/19 and plots the geotherm.
# Updated 2019.01.29 CH.


## INPUT CONSTANTS AND VALUES --------------------------------------------------

# Input constants and values. "input value" is used as a placeholder.
# Convert values to the correct units for calculations.

# Material properties of the system.
  C = "input value"    # specific heat or heat capacity [J/kg*K]
  rho = "input value"  # density [kg/m^3]

# Temperature.
  T.s.C = 20            # temperature at the surface [C]
  T.s = T.s.C + 273.15  # temperature at the surface [K]
  T.z = "input value"   # temperature at depth z [K]
  T.b.C = 1600          # temperature at depth D [C]
  T.b = T.b.C + 273.15  # temperature at depth D [K]
  
# Layer thickness or depth.
  z = "input value"        # depth [m]
  delta.z = "input value"  # layer thickness [m]
  D.km = 40                # thickness of the layer of interest
  D = D.km * 1000          # thickness of the layer of interest [m]
  
# Heat flow.
  Q = "input value"                       # heat flow [J/(m*s^2) or W/m^2]
  Q.s = "input value"                     # surface heat flow [W/m^2]
  # Oceanic surface heat flow ranges from 59 to 95 mW/m^2.
  Q.ocean = median(c(59, 95)) / 1000     # median value [W/m^2]
  # Continental surface heat flow ranges from 38 to 200 mW/m^2.
  Q.continent = mean(c(38, 200)) / 1000  # median value [W/m^2]
  
# Heat.
  q = "input value"  # heat [J]

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
  A.uW = 4      # heat production [μW/m^3]
  A = A.uW * 10^-6  # heat production [W/m^3]
  # Adjust variables for second layer of the model.
  A.uW.mantle = 0      # heat production [μW/m^3]
  A.mantle = A.uW * 10^-6  # heat production [W/m^3]

# Advection.
  U = "input value" # advection [m/s]
  U.z = "input value" # advection at depth z


# Thermal gradient.
  dT.dz = "input value"  # thermal gradient [K/m]
  
# Thermal conductivity.
  # Reasonable values: k is between 1.5 and 3.0 J/(s*m*K).
  k = "input value"  # thermal conductivity [J/(s*K*m) or W/(m*K)]
  
# Thermal diffusivity.
  K = k / (C * rho)  # thermal diffusivity [m^2/s]
  

## CALCULATIONS ----------------------------------------------------------------

# Assumptions:
  # 1. U.z = 0  No advection.
  # 2. A is constant and set above.
  # 3. Q.s is equal to the median value of surface heat flow for continents.
  Q.s = Q.continent                    # surface heat flow [W/m^2]
  # 4. k is between 1.5 and 3.0 J/(s*m*K) (input above).
  # 5. The lithosphere is 100 km thick (input above).

  
## SOLUTION 1  

# Boundary condtions:
  # 1. T = T.s at z = 0 (constant surface temperature)
  # 2. T = T.b at z = D (constant basal temperature)

# For 0 ≤ z < D, calculate temperature for a given z value for a range of k 
# values.
  # Set thermal conductivity.
  # Reasonable values: k is between 1.5 and 3.0 J/(s*m*K).
  k <- seq(from = 1.5, to = 3.0, by = 0.5)
  # Create a function to calculate T.z for a constant surface temperature and 
  # constant basal temperature.
  Tz.TsTb <- function(k){
    # Create a data frame with a column of z values from 0 to D in m.
    DF <- data.frame("z" = seq(from = 0, to = D, by = 1000))
    # Calculate T.z assuming a constant basal temperature.
    DF$T.z <- T.s +
      (((T.b - T.s) + ((A * (D ^ 2)) / (2 * k))) / D) * DF$z -
      ((A * (DF$z ^ 2)) / (2 * k))
  }
  # Use sapply to loop over different values of k.
  DF.T.z.TsTb <- as.data.frame(sapply(k, Tz.TsTb))
  # Rename columns based on k values.
  colnames(DF.T.z.TsTb) <- paste("k = ", k, sep = "")
  # Add column of depth for plotting.
  DF.T.z.TsTb$z <- seq(from = 0, to = D, by = 1000)
  
  
# For z > D, calculate temperature for a given z value for a range of k values.
  # Set thermal conductivity.
  # Reasonable values: k is between 1.5 and 3.0 J/(s*m*K).
  k <- seq(from = 1.5, to = 3.0, by = 0.5)
  # Create a function to calculate T.z for a constant surface temperature and 
  # constant basal temperature.
  Tz.TsTb.belowD <- function(k){
    # Create a data frame with a column of z values from 0 to D in m.
    DF <- data.frame("z" = seq(from = D, to = 2 * D, by = 1000))
    # Calculate T.z assuming a constant basal temperature.
    DF$T.z <- T.b +
      (DF$z - D) * ((((T.b - T.s) + ((A.mantle * (D ^ 2)) / (2 * k))) / D) - ((A.mantle * D) / k))
  }
  # Use sapply to loop over different values of k.
  DF.T.z.TsTb.belowD <- as.data.frame(sapply(k, Tz.TsTb.belowD))
  # Rename columns based on k values.
  colnames(DF.T.z.TsTb.belowD) <- paste("k = ", k, sep = "")
  # Add column of depth for plotting.
  DF.T.z.TsTb.belowD$z <- seq(from = D, to = 2 * D, by = 1000)

  
## SOLUTION 2 

# Boundary condtions:
  # 1. T = T.s at z = 0 (constant surface temperature)
  # 2. Q = Q.s at z = D (constant surface heat flow)
  
# For 0 ≤ z < D, calculate temperature for a given z value for a range of k 
# values.
  # Set thermal conductivity.
  # Reasonable values: k is between 1.5 and 3.0 J/(s*m*K).
  k <- seq(from = 1.5, to = 3.0, by = 0.5)
  # Create a function to calculate T.z for a constant surface heat flow value.
  Tz.TsQs <- function(k){
    # Create a data frame with a column of z values from 0 to D in m.
    DF <- data.frame("z" = seq(from = 0, to = D, by = 1000))
    # Calculate T.z assuming a constant surface temperature and constant surface
    # heat flow.
    DF$T.z <- T.s +
     ((Q.s / k) * DF$z) -
      ((A * (DF$z ^ 2)) / (2 * k))
    }
  # Use sapply to loop over different values of k.
  DF.T.z.TsQs <- as.data.frame(sapply(k, Tz.TsQs))
  # Rename columns based on k values.
  colnames(DF.T.z.TsQs) <- paste("k = ", k, sep = "")
  # Add column of depth for plotting.
  DF.T.z.TsQs$z <- seq(from = 0, to = D, by = 1000)
  
# For z > D, calculate temperature for a given z value for a range of k 
# values.
  # Set thermal conductivity.
  # Reasonable values: k is between 1.5 and 3.0 J/(s*m*K).
  k <- seq(from = 1.5, to = 3.0, by = 0.5)
  # Create a function to calculate T.z for a constant surface heat flow value.
  Tz.TsQs.belowD <- function(k){
    # Create a data frame with a column of z values from 0 to D in m.
    DF <- data.frame("z" = seq(from = D, to = 2 * D, by = 1000))
    # Calculate T.z below depth D assuming a constant surface temperature and 
    # constant surface heat flow.
    DF$T.z <- T.s +
      (((Q.s * D) / k) - ((A * (D ^ 2)) / (2 * k))) +
      ((Q.s - (A * D)) / k) * (DF$z - D)
  }
  # Use sapply to loop over different values of k.
  DF.T.z.TsQs.belowD <- as.data.frame(sapply(k, Tz.TsQs.belowD))
  # Rename columns based on k values.
  colnames(DF.T.z.TsQs.belowD) <- paste("k = ", k, sep = "")
  # Add column of depth for plotting.
  DF.T.z.TsQs.belowD$z <- seq(from = D, to = 2 * D, by = 1000)


## PLOT ------------------------------------------------------------------------

# Setup.
  # Load ggplot2.
  library(ggplot2)
  # Load reshape2.
  library(reshape2)
  
## PLOT SOLUTION 1 (0 ≤ z < D)
  
  # Melt dataframe to get it into ideal format for plotting.
  melted.DF.T.z.TsTb <- melt(DF.T.z.TsTb, id.vars = "z")
  
  # Plot the steady state geotherm calculated with a constant basal T.
  ggplot(data = melted.DF.T.z.TsTb,
         aes(x = value - 273.15,
             y = z / 1000,
             color = variable)) +
    geom_path() +
    scale_y_reverse() +
    theme_bw() +
    labs(title = paste("Steady state geotherm (constant T.s, constant T.b), A = ", A.uW, " μW/m^3", sep = ""),
         color = "Thermal conductivity",  # Label legend.
         x = "Temperature (°C)",  # Label x axis.
         y = "Depth (km)")  # Label y axis.

  
## PLOT SOLUTION 1 (z > D)  
  
  # Melt dataframe to get it into ideal format for plotting.
  melted.DF.T.z.TsTb.belowD <- melt(DF.T.z.TsTb.belowD, id.vars = "z")
  
  # Plot the steady state geotherm calculated with a constant basal T.
  ggplot(data = melted.DF.T.z.TsTb.belowD,
         aes(x = value - 273.15,
             y = z / 1000,
             color = variable)) +
    geom_path() +
    scale_y_reverse() +
    theme_bw() +
    labs(title = paste("Steady state geotherm (constant T.s, constant T.b), A = ", A.uW, " μW/m^3", sep = ""),
         color = "Thermal conductivity",  # Label legend.
         x = "Temperature (°C)",  # Label x axis.
         y = "Depth (km)")  # Label y axis.
  
  
## PLOT SOLUTION 2 (0 ≤ z < D)
  
  # Melt dataframe to get it into ideal format for plotting.
  melted.DF.T.z.TsQs <- melt(DF.T.z.TsQs, id.vars = "z")
  
  # Plot the steady state geotherm calculated with a constant basal T.
  ggplot(data = melted.DF.T.z.TsQs,
         aes(x = value - 273.15,
             y = z / 1000,
             color = variable)) +
    geom_path() +
    scale_y_reverse() +
    theme_bw() +
    labs(title = paste("Steady state geotherm (constant T.s, constant Q.s), A = ", A.uW, " μW/m^3", sep = ""),
         color = "Thermal conductivity",  # Label legend.
         x = "Temperature (°C)",  # Label x axis.
         y = "Depth (km)")  # Label y axis.

## PLOT SOLUTION 2 (z > D)
  
  # Melt dataframe to get it into ideal format for plotting.
  melted.DF.T.z.TsQs.belowD <- melt(DF.T.z.TsQs.belowD, id.vars = "z")
  
  # Plot the steady state geotherm calculated with a constant basal T.
  ggplot(data = melted.DF.T.z.TsQs.belowD,
         aes(x = value - 273.15,
             y = z / 1000,
             color = variable)) +
    geom_path() +
    scale_y_reverse() +
    theme_bw() +
    labs(title = paste("Steady state geotherm (constant T.s, constant Q.s), A = ", A.uW, " μW/m^3", sep = ""),
         color = "Thermal conductivity",  # Label legend.
         x = "Temperature (°C)",  # Label x axis.
         y = "Depth (km)")  # Label y axis.
  
  