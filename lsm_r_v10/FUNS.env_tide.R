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

  # replace sst from sat by sst from logger data
  w$sst <- approx(wat$time, wat$temp, w$time, method = "linear", rule = 2)$y %>% "+"(273.15) %>% round(2)

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
