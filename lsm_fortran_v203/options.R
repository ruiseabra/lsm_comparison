# for use with lsm source code version 203

## options for the LSM
opt <- list()

# Total # of simulation time steps
opt$NRUN               = length(timestamps)

# Time step for integration in sec (<= 3600)
opt$DT                 = t_res

# Height (above ground) of the forcing wind vector (m)
opt$ZLVL               = 10

# Filename of atmospheric data used for input forcing
opt$FORCING_FILE       = basename(forcing)

# Thickness of each soil layers (m) # DON'T USE MORE THAN 20 LAYERS !!!!!!
opt$SLDPTH             = c(0.001, round(0.01 * (1.5^(0:13)), 3))

# Number of soil layers (2-20)
opt$NSOIL              = length(opt$SLDPTH)

# Annual constant bottom boundary soil temperature (K)
opt$ANNUAL_BOTTOM_TEMP = 273.15 + 10

# Initial temperature at layers
opt$STC                = rep(opt$ANNUAL_BOTTOM_TEMP, length(opt$SLDPTH))

# Initial total moisture at layers
opt$SMC                = rep(0, length(opt$SLDPTH))

# Initial liquid moisture at layers
opt$SH2O               = rep(0, length(opt$SLDPTH))

# Roughness length (m)
opt$ROUGHNESS          = 0.005

# Albedo (snow free albedo)
opt$ALBEDO             = 0.2

# Bed depth (# layers)
opt$BEDDEPTH           = 5

# Fraction of contact with substrate
opt$CONTACT            = 1

# Emissivity (fraction)
opt$EMISSIVITY         = 0.7

# Animal body difusivity
opt$BODY_DIFUSIVITY    = 4

# Heat capacity - ROCK 2.0E6
opt$HTCP_ROCK          = "4.0E6"

# Heat capacity - ANIMAL (~ OYSTER 3.52E6)
opt$HTCP_ANIMAL        = "4.52E6"
