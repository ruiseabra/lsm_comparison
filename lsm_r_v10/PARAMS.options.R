# target layer ----
# output temperature of layer...
NLAYER <- 1

# soil layer depths ----
# thickness of each soil layers (m) 
# min 2, max 20
# avoid sharp changes in the depth of contiguous layers
SLDPTH <- c(0.001, round(0.01 * (1.5^(0:13)), 3))

# zlvl ----
# height (m) above ground of the atmospheric forcing variables
ZLVL <- 10

# surface roughness ----
# in meters
# == Z0
ROUGHNESS <- 0.005

# slope type ----
# used to estimate baseflow runoff out of bottom layer
# ???
SLOPE <- 1

# bottom temp ----
# annual constant bottom boundary soil temperature
# in Kelvin
BOTTOM_TEMP <- 273.15 + 10

# albedo ----
# background snow free albedo
# fraction (0-1) of radiation reflected
# in earlier versions of the noah_lsm this was a vector of values per month, but now it is assumed constant throughout the year (but algal cover...?)
ALBEDO <- 0.2

# emissivity
# fraction (0-1) of radiation emited
EMISSIVITY <- 0.7

# bed depth ----
# number of layers corresponding to the animal
BED_DEPTH <- 5

# contact ----
# fraction of contact with substrate (0-1)
CONTACT <- 1

# body difusivity ----
# animal body difusivity
# ?dimensions?
BODY_DIFUSIVITY <- 4

# heat capacity ----
# NOTE: there are other heat capacity variables set in the constants file, and
#        they differ in magnitude by 1000
#       must try to make sense of the values

# rock (~ 2.0E6)
HTCP_ROCK <- 4e6
# animal (~ oyster 3.52e6)
HTCP_ANIMAL <- 4.52e6

# thermal conductivity
# ROCKDF1N (DSW set it to 2.1)
# wikipedia values for granite range from 1.73 to 3.98, most common around 1.8+-0.1
ROCKDF1N <- 2.1

# ---------------------------------------- #
# .. other ----
# time interval, timestamps, nrun
T_RANGE    <- ymd_h(T0, T1)
INTERVAL   <- interval(ymd_h(T0), ymd_h(T1))
TIMESTAMPS <- seq.POSIXt(ymd_h(T0), ymd_h(T1), DT)
NRUN       <- length(TIMESTAMPS) # total # of simulation time steps

# nsoil
# number of soil layers (at least 2)
NSOIL <- length(SLDPTH)

# zsoil
# depth (negative) below ground from top skin surface
# to bottom of each soil layer
ZSOIL <- cumsum(-SLDPTH)
