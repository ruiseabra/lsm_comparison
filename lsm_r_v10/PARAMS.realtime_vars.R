ZEROS <- rep(0, NSOIL)
VARLIST <- c()
VARLIST <- ls()

## HUMIDITY ----------#
# mixing ratio at height opt$zlvl above ground (Kg Kg-1)
Q2 <- 0

# saturation mixing ratio at height opt$zlvl above ground (Kg Kg-1)
Q2SAT <- 0

# slope of saturation specific humidity curve (Kg Kg-1)
DQSDT <- 0


## TEMPERATURE ----------#
# temperature at layers (at middle height, K)
STC <- rep(BOTTOM_TEMP, NSOIL)

# ground/canopy/snowpack effective skin temperature (K)
TSKIN <- BOTTOM_TEMP

# bottom soil temperature (local yearly mean surface air temperature; K)
TBOT <- BOTTOM_TEMP

# air potential temperature (K) at height opt$zlvl above ground
TH2 <- 0

# virtual temperature (???) for SFLX and PENMAN
T1V <- T2V <- TH2V <- T14 <- T24 <- 0

# effective snow-ground surface temperature
T12 <- 0

# soil surface temperature (K)
TSOIL <- 0


## FLUX AND RADIATION ----------#
 # actual latent heat flux (W m-2; negative if up from surface)
ETA <- 0

# surface exchange coefficient for momentum (m s-1)
#  (technically a conductance since it has been multiplied by wind speed)
#  (also known as surface drag coefficient)
CM <- 1e-4

# surface exchange coefficient for heat and moisture (m s-1)
#  (technically a conductance since it has been multiplied by wind speed)
CH <- 1e-4

# companion coefficient to CH, is the CH times air density and CP
RCH <- 0

# subsurface heat flux (soil heat flux)
#  SSOIL > 0: warm the surface (night time)
#  SSOIL < 0: cool the surface (day time)
SSOIL <- 0

 # total downward radiation (solar + longwave)
FDOWN <- 0

# Right-Hand Side Tendency Terms for subsurface heat flux
RHSTS <- ZEROS

# vertical difference of the heat flux at top and bottom of first 
#  soil layer (impacts the freeze <-> thaw balance)
QTOT <- 0


## EVAPORATION AND MOISTURE ----------#
# total soil moisture content at layers (volumetric fraction)
SMC <- ZEROS

# direct evaporation from soil surface (W m-2)
EDIR <- 0

# effective EDIR (different units)
EDIR1 <- 0

# plant transpiration from a particular root (soil) layer (W m-2)
ET <- ZEROS

# effective ET  (different units)
ET1 <- 

# modeled actual evapo-transpiration
ETA1 <- 0

# potential evaporation (ETP < 0 leads to dew formation)
ETP <- 0

# effective ETP  (different units)
ETP1 <- 0

# ratio of actual / potential evaporation (dimensionless)
BETA <- 0

# Right-Hand Side Tendency Terms for subsurface water flux
RHSTT <- ZEROS


## SNOW AND RELATED ----------#
# dew fall (or frostfall for temperature below 0C) (m)
DEW <- 0

# precipitation logical: snowing?
#  (i.e. raining and air temperature below 0C)
SNOWNG <- FALSE

# precipitation logical: freezing rain?
#  (i.e. raining and air temperature above 0C, but ground temperature below 0C)\
FRZGRA <- FALSE


## OTHER ----------#
# arithmetic mean of thermal difusivity of top soil layer (parallel flow)
DF1A <- 0

# final thermal difusivity of surface mediums
DF1 <- 0

# liquid rain that can wholely or partially infiltrate the soil
RAIN1 <- 0

# new amount of liquid water
XH2O <- 0

# actual surface roughness length
Z0 <- ROUGHNESS

# tri-diagonal matrix coefficients
AI <- BI <- CI <- ZEROS


## UNKNOWN PURPOSE ----------#
EPSCA <- 0
SH2OFG <- 0
RR <- 0

## SHFLX
YY <- 0
ZZ1 <- 0

VARLIST <- setdiff(ls(), VARLIST)
