########################-
## collect tide data ###-
########################-
# xy = list(lon = lon, lat = lat)
# T_RANGE = POSIXct date:time range (2 values)
# DT = every 'DT' seconds
########################-
fes.tides <- function(LOC, T_RANGE, DT) {
  Sys.setenv(HDF5_DISABLE_VERSION_CHECK = "2")
  # prepare
  ORIGIN  <- ymd("1950-01-01")
  t_range <- (T_RANGE + c(0, DT)) %>%
    julian(origin = ORIGIN) %>%
    as.numeric %>%
    formatC(format = "f")
  dt <- DT / 60
  CALL  <- str_c("fes_slev", LOC$lat, LOC$lon, t_range[1], t_range[2], dt, sep = " ")
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
  tides <- filter(tides, time %within% interval(T_RANGE[1], T_RANGE[2]))
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
# 2. interpolate to dt
# 3. get tide height and compute submersion
# 4. use sfc pressure to adjust tide height
# ?? read wave data and compute wave run-up
#------------------------------#
forcing.data <- function() {
  ## read weather data
  #forcing_cols <- c("time", "wind", "air", "sst", "sw", "lw", "rh", "pres", "rain", "wave")
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
  # pres - surface pressure (hPa)
  # rain - precipitation rate (mm/h, kg/m2/h)
  # wave - NOT YET IMPLEMENTED sig or max wave height (m)
  
  if (!identical(sort(colnames(w)), sort(forcing_cols))) stop("colnames(w) does not match requirements")
  w <- filter(w, time %within% interval(T_RANGE[1], T_RANGE[2]))
  w <- w[, forcing_cols]

  # interpolate to the desired time resolution
  res <- with(w, difftime(time[1], time[2], units = "sec")) %>% abs 
  if (DT != res) {
    t2 <- seq.POSIXt(first(T_RANGE), last(T_RANGE), by = DT)
    tmp <- merge(zoo(select(w, -time), w$time), zoo(, t2))
    tmp <- tmp[t2, ] # important only if DT > res
    tmp <- as.data.frame(coredata(na.fill(tmp, "extend")))
    w <- as_tibble(cbind(t2, tmp))
    colnames(w) <- forcing_cols
  }
  
  # get tide data
  tides <- fes.tides(LOC, T_RANGE, DT)$tide
  # barometric pressure correction (+10 hPa = -10 cm; REF = 1013 hPa)
  # this approach is crude but can be extended to the future as well as the past, with the same level of bias
  tides <- tides - (w$pres - 1013)
  # 1 = underwater, 0 = out-of-water
  w$tide <- ifelse(tides > LOC$height, 1, 0)
  # include waverunup
  # include atmospheric effects (pressure, wind surge)
  
  # convert surface pressure to Pa
  w$pres <- w$pres * 100
  
  # fun DQSDT (within SatMix) is only valid for air temperatures between 173 and 373 K (-100 to +100 C)
  if (any(w$air < 173 | w$air > 373)) stop("DQSDT")

  
  # clean, round and format
  w$wind <- round(w$wind, 2)
  w$air  <- round(w$air,  1)
  w$sst  <- round(w$sst,  1)
  w$sw   <- round(w$sw,   1)
  w$lw   <- round(w$lw,   1)
  w$rh   <- round(w$rh,   1)
  w$pres <- round(w$pres, 0)
  w$rain <- round(ifelse(w$rain < 0, 0, w$rain), 2)

  w
}
########################-
########################-
########################-