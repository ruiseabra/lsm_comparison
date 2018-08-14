# for use with lsm source code version 203

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
forcing.file <- function(loc, t_range, t_res, output.path) {
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

  # replace sst from sat by sst from logger data
  w$sst <- approx(wat$time, wat$temp, w$time, method = "linear", rule = 2)$y %>% "+"(273.15) %>% round(2)

  # clean, round and format
  w$wind <- sprintf("%6.2f", w$wind)
  w$air  <- sprintf("%6.1f", w$air)
  w$sst  <- sprintf("%6.1f", w$sst)
  w$sw   <- sprintf("%7.1f", w$sw)
  w$lw   <- sprintf("%7.1f", w$lw)
  w$rh   <- sprintf("%6.1f", w$rh)
  w$pres <- sprintf("%9.1f", w$pres)
  w$rain <- sprintf("%7.2f", ifelse(w$rain < 0, 0, w$rain))
  w$tide <- sprintf("%2i"  , w$tide)

  # export focing file to temporary folder
  write.table(x = select(w, -time), file = output.path, quote = FALSE, col.names = FALSE, row.names = FALSE)
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
  lsm1 <- dir(str_c(dirname(path), "/source"), full.names = TRUE, pattern = "203")
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
