# for use with lsm source code version 202

########################-
## collect tide data ###-
########################-
# xy = list(lon = lon, lat = lat)
# t_range = POSIXct date:time range (2 values)
# t_res = every 't_res' seconds
########################-
fes.tides <- function(loc, t_range, t_res) {
  Sys.setenv(HDF5_DISABLE_VERSION_CHECK = "2")
  # prepare
  ORIGIN  <- ymd("1950-01-01")
  T_RANGE <- (t_range + c(0, t_res)) %>%
    julian(origin = ORIGIN) %>%
    as.numeric %>%
    formatC(format = "f")
  T_RES <- t_res / 60
  CALL  <- str_c("fes_slev", loc$lat, loc$lon, T_RANGE[1], T_RANGE[2], T_RES, sep = " ")
  # run fes_slev
  tides <- system(CALL, intern = TRUE)[-(1:2)] %>%
    str_split(",") 
  # extract timestamps
  times <- map_chr(tides, 1) %>% 
    as.numeric %>% 
    "*"(., (24 * 3600)) %>% 
    round
  times <- (times - (times %% 60)) %>% 
    as.POSIXct(origin = ORIGIN)
  times <- times - (as.numeric(times) %% 60)
  steady <- diff(times) %>%
    unique %>%
    length %>%
    "=="(., 1)
  if (!steady) stop("the period of 'times' is irregular")
  # extract tide elevation
  tides <- map_chr(tides, 2) %>% as.numeric
  # combine
  tides <- tibble(time = times, tide = tides)
  # filter to ensure that the data return does not exceed the t_range supplied
  tides <- filter(tides, time %within% interval(t_range[1], t_range[2]))
  # return
  tides
}
########################-
########################-
########################-


########################-
## make forcing file ###-
########################-
# 1. read weather data
# 2. interpolate to t_res
# 3. get tide height and compute submersion
# 4. use sfc pressure to adjust tide height
# ?? read wave data and compute wave run-up
# 5. format output
# 6. export forcing.in
#------------------------------#
forcing.file.191 <- function(loc, t_range, t_res, output.path) {
  ## read weather data
  forcing_cols <- c("time", "wind", "air", "sst", "sw", "lw", "rh", "pres", "rain")
  
  wfile <- dir("data/", full.names = TRUE, pattern = "weather")
  if (length(wfile) != 1) stop("there must be one - and only one - with 'weather' on its filename")
  load(wfile)
  
  ### must be a tibble with the following columns (colnames must be respected):
  # time - YYYY-mm-dd HH:MM
  # wind - wind speed (m/s) [alternativelly supply u and v components (m/s)]
  # air  - surface air temperature (K)
  # sst  - sea surface temperature (K)
  # sw   - incoming short wave radiation (W/m2)
  # lw   - incoming  long wave radiation (W/m2)
  # rh   - relative humidity (%)
  # pres - surface pressure (Pa)
  # rain - precipitation rate (mm/h, kg/m2/h)
  # wave - NOT YET IMPLEMENTED sig or max wave height (m)
  
  if (!identical(sort(colnames(w)), sort(forcing_cols))) stop("colnames(w) does not match requirements")
  w <- filter(w, time %within% interval(t_range[1], t_range[2]))
  w <- w[, forcing_cols]
  
  # interpolate to the desired time resolution
  res <- with(w, difftime(time[1], time[2], units = "sec")) %>% abs 
  if (t_res != res) {
    t2 <- seq.POSIXt(first(t_range), last(t_range), by = t_res)
    tmp <- merge(zoo(select(w, -time), w$time), zoo(, t2))
    tmp <- tmp[t2, ] # important only if t_res > res
    tmp <- as.data.frame(coredata(na.fill(tmp, "extend")))
    w <- as_tibble(cbind(t2, tmp))
    colnames(w) <- forcing_cols
  }
  
  # get tide data
  tides <- fes.tides(loc, t_range, t_res)$tide
  # barometric pressure correction (+10 hPa = -10 cm; REF = 1013 hPa)
  # this approach is crude but can be extended to the future as well as the past, with the same level of bias
  tides <- tides - (w$pres - 1013)
  # 1 = underwater, 0 = out-of-water
  w$tide <- ifelse(tides > loc$height, 1, 0)
  # include waverunup
  # include atmospheric effects (pressure, wind surge)
  
  w$rain <- ifelse(w$rain < 0, 0, w$rain)

  # Column order: jday, hhmm, wind, airT, RH, pressure, shortwave, longwave, rain, sst, tideflag
  
  w <- tibble(
    jday = yday(w$time),
    hhmm = str_c(" ", formatC(hour(w$time), width = 2, flag = "0"), formatC(minute(w$time), width = 2, flag = "0")),
    wind = sprintf("%6.2f", w$wind),
    air  = sprintf("%6.1f", w$air),
    rh   = sprintf("%6.1f", w$rh),
    pres = sprintf("%9.1f", w$pres),
    sw   = sprintf("%7.1f", w$sw),
    lw   = sprintf("%7.1f", w$lw),
    rain = sprintf("%7.2f", w$rain),
    sst  = sprintf("%6.1f", w$sst),
    tide = sprintf("%2i"  , w$tide))
  
  # export focing file to temporary folder
  write.table(w, file = output.path, quote = FALSE, col.names = FALSE, row.names = FALSE)
}
########################-
########################-
########################-

########################-
## break long strings ##-
########################-
# transform opt variables that would extend
#  beyond the char lim of a fortran line
#  into a string with multiple lines
#------------------------------#
fortran.multiLine <- function(x) {
  n <- 4
  X <- c()
  while (length(x) > n) {
    X <- c(X, str_c(str_c(x[1:4], collapse = ", "), ",\n"))
    x <- x[-(1:4)]
  }
  if (length(x)) X <- c(X, str_c(x[1:length(x)], collapse = ", "))
  if (length(X) > 1) X[2:length(X)] <- str_c("     & ", X[2:length(X)])
  str_c(X, collapse = "")
}
########################-
########################-
########################-

## write.controlfile ###
write.controlfile <- function(path) {
  fn <- str_c(path, "controlfile.1")
  sink(fn)
  
  # there must be enough spaces between the value and its description or an error like the one below occurs
  ### At line 3025 of file NoahTest_v1.91.f (unit = 21, file = 'controlfile.1')
  ### Fortran runtime error: Bad real number in item 0 of list input
  # thus, we use strXpand to maintain sufficient distance
  
  cat("Model Configuration:\n")
  cat("----------------- \n")
  cat(strXpand(loc$lat),            "LATITUDE..(N > 0.00 (+); S < 0.00 (-))\n")
  cat(strXpand(loc$lon),            "LONGITUDE.(W > 0.00 (+); E < 0.00 (-))\n")
  cat(strXpand(-1),                 "IBINOUT...(+/-) Output type: +1=Binary(GrADS), -1=ASCII(*.TXT)\n")
  cat(strXpand(yday(ymd_h(t0))),    "JDAY......Initial julian day of simulation (1-366)\n")
  cat(strXpand("0000"),             "TIME......Initial time \"hhmm\"\n")
  cat(strXpand(1),                  "NCYCLES...Cycles the forcing data (useful for spin-up runs)\n")
  cat(strXpand(365),                "SYDAYS....Days in spin-up year\n")
  cat(strXpand(".FALSE."),          "L2nd_data.Use 2nd forcing data file (useful after spin-up runs)\n")
  cat(strXpand(length(timestamps)), "NRUN......Total # of simulation time steps\n")
  cat(strXpand(dt),                 "DT........Time step for integration in sec (<= 3600)\n")
  cat(strXpand(opt$NSOIL),          "NSOIL.....Number of soil layers (2-20)\n")
  cat(strXpand(opt$ZLVL),           "Z.........Height (above ground) of the forcing wind vector (m)\n")
  cat(str_c(opt$SLDPTH, collapse = "  "), "   K=1,NSOIL...thickness of each soil layer (m)\n")
  cat("--------------------------------------------------\n")
  cat("Filenames of atmospheric data used for input forcing (1 and 2):\n")
  cat("-------------------------------------------------- \n")
  cat("noah.in\n")
  cat("noah.in\n")
  cat("-------------------------------------------------------------\n")
  cat("Integer indexes designating soil type, veg type and slope type:\n")
  cat("-------------------------------------------------------------\n")
  cat(strXpand(10),   "SOILTYP...Soil type index 1-9\n")
  cat(strXpand(14),   "VEGTYP....Vegetation type index 1-13\n")
  cat(strXpand(1),    "SLOPETYP..Slope type index 1-9\n")
  cat("----\n")
  cat("Monthly ALBEDO (snow free albedo):\n")
  cat("J  F  M  A  M  J  J  A  S  O  N  D\n")
  cat(str_c(rep(opt$ALBEDO, 12), collapse = " "), "\n")
  cat("----\n")
  cat("Monthly SHDFAC (green vegetation fraction):\n")
  cat("J  F  M  A  M  J  J  A  S  O  N  D\n")
  cat(str_c(rep("0.0", 12), collapse = " "), "\n")
  cat("----\n")
  cat(strXpand(0.75), "SNOALB....Max albedo over very deep snow\n")
  cat(strXpand(0), "SEA ICE...Sea ice flag (keep as integer 0 to designate non-sea)\n")
  cat("------------------ \n")
  cat("Physical parameters:\n")
  cat("------------------ \n")
  cat(strXpand(opt$ANNUAL_BOTTOM_TEMP), "TBOT......Annual constant bottom boundary soil temperature (K)\n")
  cat("---------------------- \n")
  cat("Initial state variables:\n")
  cat("---------------------- \n")
  cat(strXpand(opt$ANNUAL_BOTTOM_TEMP), "T1........Initial skin temperature (K)\n")
  cat(str_c(rep(opt$ANNUAL_BOTTOM_TEMP, opt$NSOIL), collapse = " "), "  STC (temperature at layers)\n")
  cat(str_c(rep(0, opt$NSOIL), collapse = " "), "  SMC (total moisture at layers)\n")
  cat(str_c(rep(0, opt$NSOIL), collapse = " "), "  SH2O (liquid moisture at layers)\n")
  cat(strXpand(0), "CMC.......Initial canopy water content (m)\n")
  cat(strXpand(0), "SNOWH.....Initial actual snow depth (m)\n")
  cat(strXpand(0), "SNEQV.....Initial water equiv snow depth (m)\n")
  cat("-------------------------------------- \n")
  cat("----- END OF READABLE CONTROLFILE -----------------------------------------------\n")
  
  sink()
}

## write.namelist ###
write.namelist <- function(path) {
  fn1 <- str_c(path, "namelist.txt")
  fn2 <- str_c(path, "namelist_filename.txt")

  txt <- "$SOIL_VEG\nLPARAM = .TRUE. ,\n"
  txt <- str_c(txt, "Z0_DATA(14) = ", opt$ROUGHNESS, "\n")
  txt <- str_c(txt, "BEDDEPTH_DATA = ", opt$BEDDEPTH, " ,\n")
  txt <- str_c(txt, "CNTCT_DATA = ",  formatC(opt$CONTACT, digits = 2, format = "f"), " ,\n")
  txt <- str_c(txt, "EMISSIVITY_DATA = ", formatC(opt$EMISSIVITY, digits = 2, format = "f"), " ,\n")
  txt <- str_c(txt, "$END\n")
  
  write(txt, file = fn1)
  write(basename(fn1), file = fn2)
}

## expand string to consistent length ###
strXpand <- function(x, L = 10) {
  x <- as.character(x)
  str_c(x, str_c(rep(" ", L - nchar(x)), collapse = ""))
}


########################-
## edit fortran file  ##-
########################-
# 1. read lsm parameters
# 2. edit lsm source code
# 3. compile
#------------------------------#
compile.lsm.191 <- function(path, layer) {
  file.remove(dir(path, full.names = TRUE, pattern = "lsm"))

  # original source code (remains unaltered)
  lsm1 <- dir(str_c(dirname(path), "/source"), full.names = TRUE, pattern = "191mod")
  # edited source code
  lsm2 <- str_c(tmp, "lsm.f")
  # compiled lsm
  lsm3 <- str_c(tmp, "lsm")
  
  if (length(lsm1) != 1) stop("there must be one, and only one, file of the same lsm version in the 'source' folder")
  
  source(str_c(dirname(path), "/options.R"))
  write.namelist(path)
  write.controlfile(path)
  
  ## edit source file
  invisible(file.copy(lsm1, lsm2, overwrite = TRUE))
  x <- read_lines(lsm2)
  x <- gsub("XMUSSELDF1NX", opt$BODY_DIFUSIVITY, x)
  x <- gsub("XROCKHCPTX",   opt$HTCP_ROCK, x)
  x <- gsub("XMUSSELHCPTX", opt$HTCP_ANIMAL, x)
  x <- gsub("XLAYERX", layer, x)
  write_lines(x, path = lsm2)
  
  # compile
  system(str_c("gfortran ", lsm2, " -o ", lsm3))
  
  basename(lsm3)
}
########################-
########################-
########################-