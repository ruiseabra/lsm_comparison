run_lsm <- function(type) {
  vn  <- str_c("lsm_", type)
  dn  <- str_c(vn, "/")
  fn  <- str_c(dn, "t.", type, ".RData")
  fun <- get(str_c("lsm.", type))
  if (file.exists(fn)) {
    load(fn)
  }else{
    x <- fun(dn, 1, t0, t1, dt, loc)
    save(x, file = fn)
  }
  assign(vn, x, envir = .GlobalEnv)
}

lsm.r_v10 <- function(path, l, t0, t1, dt, loc) {
  print("running lsm --> r_v10 version")
  LS <- ls(envir = .GlobalEnv)

  T0  <<- t0
  T1  <<- t1
  DT  <<- dt
  LOC <<- loc

  # load parameters
  for (f in dir(path, pattern = "PARAMS.", full.names = TRUE)) source(f)

  # load functions
  for (f in dir(path, pattern = "FUNS.", full.names = TRUE)) source(f)

  # prepare forcing data
  w <<- forcing.data()

  # set up vector to store soil layer temperature at each time step
  t <- rep(NA, nrow(w))

  ### in loop
  pb <- txtProgressBar(1, NRUN, style = 3)
  for (i in 1:NRUN) {
    # i <- 1
    # grab line [i] of the forcing data tibble
    read.env(i)

    # calculate land-surface physics
    housekeeping()
    sflx()

    # store layer temperatures
    t[i] <- STC[l]

    setTxtProgressBar(pb, i)
  }
  close(pb)

  t <- xts(t - 273.15, TIMESTAMPS)

  rm(list = setdiff(ls(envir = .GlobalEnv), c(LS, "t", "w")), envir = .GlobalEnv)
  t
}

lsm.fortran_v203 <- function(path, l, t0, t1, dt, loc) {
  print("running lsm --> fortran_v203 version")
  LS <- ls(envir = .GlobalEnv)

  t_range <<- ymd_h(t0, t1)
  t_res   <<- dt
  timestamps <<- seq.POSIXt(t_range[1], t_range[2], by = t_res)

  source(str_c(path, "functions.R"))

  # set up temporary folder for running the lsm model
  tmp <<- str_c(path, "tmp/")
  unlink(tmp, recursive = TRUE)
  dir.create(tmp)

  forcing <<- str_c(tmp, "forcing.in")
  forcing.file(loc, t_range, t_res, forcing)

  lsm <- compile.lsm(tmp, layer = l)
  out <- system(str_c("cd ", tmp, "; ./", lsm), intern = TRUE, ignore.stderr = TRUE)
  t   <- xts(as.numeric(out) - 273.15, timestamps)

  unlink(tmp, recursive = TRUE)
  rm(list = setdiff(ls(envir = .GlobalEnv), c(LS, "t")), envir = .GlobalEnv)
  t
}

lsm.fortran_v191mod <- function(path, l, t0, t1, dt, loc) {
  # l <- 1; type <- "fortran_v191mod"
  print("running lsm --> fortran_v191mod version")
  LS <- ls(envir = .GlobalEnv)

  t_range <<- ymd_h(t0, t1)
  t_res   <<- dt
  timestamps <<- seq.POSIXt(t_range[1], t_range[2], by = t_res)

  source(str_c(path, "functions.R"))

  # set up temporary folder for running the lsm model
  tmp <<- str_c(path, "tmp/")
  unlink(tmp, recursive = TRUE)
  dir.create(tmp)

  forcing <<- str_c(tmp, "noah.in")
  forcing.file.191mod(loc, t_range, t_res, forcing)

  lsm <- compile.lsm.191mod(tmp, l)
  out <- system(str_c("cd ", tmp, "; ./", lsm), intern = TRUE, ignore.stderr = TRUE)

  t <- read_lines(str_c(tmp, "THERMO.TXT")) %>% as.numeric
  t <- xts(t - 273.15, timestamps)

  unlink(tmp, recursive = TRUE)
  rm(list = setdiff(ls(envir = .GlobalEnv), c(LS, "t")), envir = .GlobalEnv)
  t
}


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
## forcing data ########-
########################-
# 1. read weather data
# 2. compute submersion
#------------------------------#
forcing.data.main <- function() {
	## read weather data
	wfile <- str_c(DIR, "/weather/weather_", dt, ".RData")
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

	w$wind <- round(w$wind, 4)
	w$air  <- round(w$air,  2)
	w$sst  <- round(w$sst,  2)
	w$sw   <- round(w$sw,   2)
	w$lw   <- round(w$lw,   2)
	w$rh   <- round(w$rh,   2)
	w$pres <- round(w$pres, 1)
	w$rain <- round(w$rain, 6)

	w
}
########################-
########################-
########################-
