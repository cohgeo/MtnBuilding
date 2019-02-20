# This script calculates and plots half-space cooling models described in class
# on 2019.02.14.
# Updated 2019.02.20 CH.


## SETUP -----------------------------------------------------------------------

# Clear environment.
  rm(list = ls())

# Load required pacakages.
  # Load ggplot2.
  library(ggplot2)
  # Load reshape2.
  library(reshape2)

# Load required functions.
  source("erf.R")
  source("erfc.R")


## CREATE FUNCTIONS ------------------------------------------------------------

# Create functions that calculate temperature (T) given a position (x) and 
# time (t) for the three cases and general scenario specified in class.

  # CASE 1
  # T = 0 at all T.
  T_Case1 <- function(x, t, T0 = T0) {
    T_Case1 <- T0 * erf(x / sqrt(4 * K * t))
    return(T_Case1)
  }

  # CASE 2
  # T = 0 at t = 0.
  T_Case2 <- function(x, t, T0 = T0) {
    T_Case2 <- (T0 / 2) + ((T0 / 2) * erf(x / sqrt(4 * K * t)))
    return(T_Case2)
  }
  
  # CASE 3
  T_Case3 <- function(x, t, T1 = T1, T2 = T2) {
    T_Case3 <- (T1) + 
      ((T2 - T1) / 2) + 
      (((T2 - T1) / 2) * erf(x / sqrt(4 * K * t)))
    return(T_Case3)
  }
  
  # GENERAL CASE
  # translation along x axis
  T_general <- function(x, t, x0 = x0, T1 = T1, delT = delT) {
    T_general <- T1 + (delT / 2) + (delT / 2) * erf((x - x0) / sqrt(4 * K * t))
    return(T_general)
  }
  

## CALCULATIONS ----------------------------------------------------------------

# SETUP, INPUT CONSTANTS

  # Set a value for thermal diffusivity (kappa, K).
  K <- 100
  
  # Set a value for T0 (temperature at time 0), used for Cases 1 and 2.
  T0 <- 500
  
  # Set values for T1 and T2 (temperatures at time 0), used for Case 3.
  T1 <- 250
  T2 <- 750
  
  # Set values for x0 and delT.
  x0 <- -1
  delT <- 15
  
  # Create a vectors of x (location) and time (t) values.
  x <- list()  # Initialize vector.
  x <- seq(from = -100, to = 100, by = 1)  # Populate vector.
  t <- list()  # Initialize vector.
  t <- seq(from = 0, to = 2, by = 0.1)  # Populate vector.

  
# Calculate temperature (T) given a position (x) and time (t).
  
  # Preallocate T.results.
  T.results <- list()
  
  # Calculate T (change function depending on model)
  for (tval in seq_along(t)) {
    T.results[[tval]] <- T_Case3(x, tval, T1, T2)
    names(T.results)[tval] <- paste("t = ", t[[tval]], sep = "")
  }

  # Create a data frame of results.
  DF <- do.call(cbind.data.frame, T.results)
  
  # Add column of x values to data frame.
  DF$x <- x

  # Melt data frame for plotting.
  DF.long <- melt(DF, id = "x")
  
  # Create a data frame of values of intial condtion.
  # Case 1, Case 2.
    # InitialCondition <- data.frame("x" = x,
    #                                "value" = T0)
    # # Change positions (x) less than or equal to 0 to T = 0.
    # InitialCondition <- within(InitialCondition, value[x <= 0] <- 0)
    # # Add a row where T = T0 at x = 0.
    # InitialCondition <- rbind(InitialCondition, 
    #                           data.frame("x" = 0, "value" = T0))
  # Case 3.
    InitialCondition <- data.frame("x" = x,
                                   "value" = T2)
    # Change positions (x) less than or equal to 0 to T = 0.
    InitialCondition <- within(InitialCondition, value[x <= 0] <- T1)
    # Add a row where T = T1 at x = 0.
    InitialCondition <- rbind(InitialCondition, 
                              data.frame("x" = 0, "value" = T1))
  
  
## PLOT ------------------------------------------------------------------------

# Plot results.
  # Initialize plot.
  ggplot() +
    # Plot starting condtion.
    geom_line(data = InitialCondition,
              aes(x = x,
              y = value),
              color = "black") +
    # Plot data.
    geom_line(data = DF.long,
              aes(x = x,
                  y = value,
                  color = variable)) +
    # Change to simpler ggplot2 theme.
    theme_bw() +
    # Change legend title.
    guides(color = guide_legend(title = "t")) +
    # Title plot and label axes.
    labs(title = "Half-space cooling model",  # Title plot.
         x = "position (x)",  # Label x axis.
         y = "temperature (T)")  # Label y axis.
  
  
  
  
### SPECIFIC MODEL AND PLOT ----------------------------------------------------
  # # For a set range of x values (specified above), calculate T_Case1 for a range
  # # of t values. Save results in a data frame for plotting.
  # 
  # # Preallocate T_1 (list of results from T_Case1 function).
  # T_1 <- list()
  # 
  # # Calculate T_1.
  # for (tval in seq_along(t)) {
  #   T_1[[tval]] <- T_Case1(x, tval, T0)
  #   names(T_1)[tval] <- paste("t_", t[[tval]], sep = "")
  # }
  # 
  # # Create a data frame of T_Case1 results.
  # DF.T_1 <- do.call(cbind.data.frame, T_1)
  # 
  # # Add column of x values to data frame.
  # DF.T_1$x <- x
  # 
  # # Add the starting condtion to the data frame.
  # DF.T_1$initial <- T0
  # # Copy first row, paste into data frame as an additional row.
  # DF.T_1[(dim(DF.T_1)[1] + 1), ] <- DF.T_1[1, ]
  # # Change value of x in additional row so that at x = 0, T = 0 and T0
  # DF.T_1$initial[1] <- 0
  # 
  # 
  # # Melt data frame for plotting.
  # T_1.long <- melt(DF.T_1, id = "x")
  # 
  # # PLOT CASE 1 
  # ggplot(data = T_1.long,
  #        aes(x = x,
  #            y = value,
  #            color = variable)) +
  #   geom_line()
  
  
  
  
  
  