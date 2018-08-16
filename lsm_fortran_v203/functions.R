# for use with lsm source code version 203

########################-
## make forcing file ###-
########################-
# 1. read weather data
# 2. compute submersion
# 3. format output
# 4. export forcing.in
#------------------------------#
forcing.file <- function(loc, t_range, t_res, output.path) {
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
	# 1 = underwater, 0 = out-of-water
	w$tide <- ifelse(w$tide > loc$height, 1, 0)

	# replace sst from sat by sst from logger data
	w$sst <- approx(time(wat), as.numeric(wat$sst), time(w), method = "linear", rule = 2)$y %>%
		"+"(273.15) %>%
		round(2)

	w <- coredata(w) %>% as.data.frame

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

########################-
## edit fortran file  ##-
########################-
# 1. read lsm parameters
# 2. edit lsm source code
# 3. compile
#------------------------------#
compile.lsm <- function(path, layer = 1) {
  file.remove(dir(path, full.names = TRUE, pattern = "lsm"))

	# original source code (remains unaltered)
	lsm1 <- dir(dirname(getwd()), full.names = TRUE, pattern = "lsm_fortran")
	lsm1 <- dir(str_c(lsm1, "/source"), full.names = TRUE, pattern = "203")
  # edited source code
  lsm2 <- str_c(tmp, "lsm.f")
  # compiled lsm
  lsm3 <- str_c(tmp, "lsm")

  if (length(lsm1) != 1) stop("there must be one, and only one, file of the same lsm version in the 'source' folder")
  invisible(file.copy(lsm1, lsm2))

  source(str_c(dirname(path), "/options.R"))
  # Output temperature of layer...
  opt$NLAYER <- layer
  opt <- tibble(var = names(opt), val = opt)
  opt$var <- str_c("X", opt$var, "X")
  opt$val <- map_chr(opt$val, ~fortran.multiLine(.x))

  ## edit source file
  x <- read_lines(lsm2)
  for (i in 1:nrow(opt)) {
    var <- opt$var[i]
    val <- opt$val[i]
    x <- str_replace(x, var, val)
  }

  write(x, file = lsm2)

  # compile
  system(str_c("gfortran ", lsm2, " -o ", lsm3))

  basename(lsm3)
}
########################-
########################-
########################-
