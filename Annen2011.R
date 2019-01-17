# This script repeats the calculations in
#     Annen, C., 2011. Implications of incremental emplacement of magma bodies 
#       for magma differentiation, thermal aureole dimensions and 
#       plutonism–volcanism relationships. Tectonophysics 500, 3–10. 
#       doi:10.1016/j.tecto.2009.04.010
# to practice thermal modelling and inputting these equations.
# Updated 2019.01.17 CH.


## Setup -----------------------------------------------------------------------

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


## Eq 1. Estimate cooling time of a sill-like magma intrusion. -----------------

# Input values for variables and constants.
b = 1500 / 2      # half dimension of the magma body [m]
k = 5 * 10 ^ -7   # thermal diffusivity [m^2 / s]

# Estimate cooling time based on magma body dimensions and thermal diffusivity.
t.cool <- b ^ 2 / k          # t = cooling time [s]; Eq. 2 of Annen (2011)
t.cool.year <- t / 31557600  # t.year = cooling time [yr]


## Eq 2. Calculate solidification time of a sill-like magma intrusion. ---------

# Input values for variables and constants.
L = 300         # L = latent heat [kJ / kg]
Tm = 1000       # Tm = magma temperature [°C]
T0 = 500        # T0 = initial T of country rock before emplacement [°C]
c = 1.3         # c = specific heat [kJ / kg]
lambda = 0.564  # dimensionless constant

# Calculate solidification time of a sill-like magma intrusion (non-convecting,
# that releases latent heat during crystallization, assuming instantaneous 
# solidification below Tm).
t.solidification <- b ^ 2 / (4 * k * lambda ^ 2)      # solidifcation time [s]
t.solidification.year <- t.solidification / 31557600  # solidifcation time [yr]

# I think I would need to numerically sovle for lambda (Eq. 3) if it wasn't 
# given to me. I think this link has guidance for how to do that:
# http://rstudio-pubs-static.s3.amazonaws.com/32888_197d1a1896534397b67fb04e0d4899ae.html









