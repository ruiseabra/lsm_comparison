# for use with lsm source code version 191mod

########################-
## make forcing file ###-
########################-
# 1. read weather data
# 2. compute submersion
# 3. format output
# 4. export forcing.in
#------------------------------#
forcing.file.191mod <- function(loc, t_range, t_res, output.path) {
	## read weather data
	wfile <- str_c(DIR, "/weather/weather_", t_res, ".RData")
	if (!file.exists(wfile)) stop("weather file is missing")
	load(wfile)

	### must be a xts with the following columns (colnames must be respected):
	## index = time (YYYY-mm-dd HH:MM:SS)
	# wind - wind speed (m/s)
	# air  - surface air temperature (K)
	# sst  - sea surface temperature (K)
	# sw   - incoming short wave radiation (W/m2)
	# lw   - incoming  long wave radiation (W/m2)
	# rh   - relative humidity (0-100%)
	# pres - surface pressure (Pa)
	# rain - precipitation rate (mm/s, kg/m2/s) (always >= 0)
	# tide - height from mean water level (cm)
	# wave - NOT YET IMPLEMENTED sig or max wave height (m)

	# transform tide heights into tide in or tide out
	# 0 = underwater, 1 = out-of-water
	w$tide <- ifelse(w$tide > loc$height, 0, 1)

	# replace sst from sat by sst from logger data
	w$sst <- approx(time(wat), as.numeric(wat$sst), time(w), method = "linear", rule = 2)$y %>%
		"+"(273.15) %>%
		round(2)

	# match columns and column order for 191mod
	cols <- c("jday", "hhmm", "wind", "air", "rh", "pres", "sw", "lw", "rain", "sst", "tide")
	t <- time(w)
	w <- coredata(w) %>% as.data.frame

	w$jday <- yday(t)
	w$hhmm <- str_c("  ", formatC(hour(t), width = 2, flag = "0"), formatC(minute(t), width = 2, flag = "0"))

	# clean, round and format
	w$wind <- sprintf("%10.4f", w$wind)
	w$air  <- sprintf("%10.2f", w$air)
	w$sst  <- sprintf("%10.2f", w$sst)
	w$sw   <- sprintf("%10.2f", w$sw)
	w$lw   <- sprintf("%10.2f", w$lw)
	w$rh   <- sprintf("%10.2f", w$rh)
	w$pres <- sprintf("%10.1f", w$pres)
	w$rain <- sprintf("%10.6f", w$rain)
	w$tide <- sprintf("%4i"   , w$tide)

	w <- w[, cols]

	# export focing file to temporary folder
	write.table(x = w, file = output.path, quote = FALSE, col.names = FALSE, row.names = FALSE)
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
compile.lsm.191mod <- function(path, layer) {
	file.remove(dir(path, full.names = TRUE, pattern = "lsm"))

	# original source code (remains unaltered)
	lsm1 <- dir(dirname(getwd()), full.names = TRUE, pattern = "lsm_fortran")
	lsm1 <- dir(str_c(lsm1, "/source"), full.names = TRUE, pattern = "191mod")
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
