Sys.setenv(TZ = "UTC")
options(digits = 10)
library(tidyverse)
library(lubridate)
library(stringr)
library(xts)

del <- FALSE

source("functions.R")

t0  <- "2017-10-10 00"
t1  <- "2018-03-01 00"
dt  <- 1800
loc <- list(lon = -8.876, lat = 41.839, height = -20)

# load robolimpet data
ref <- tibble(
	path = dir("data/robolimpet/", full.names = TRUE))
ref$t <- as.list(rep(NA, nrow(ref)))

for (i in 1:nrow(ref)) {
	x <- suppressMessages(read_csv(ref$path[i], col_names = c("time", "temp"), skip = 2))
	# trim xout data to match the T_RANGE used in the LSMs
	xout <- seq(max(first(x$time), ymd_h(t0)), min(last(x$time), ymd_h(t1)), dt)
	if (length(xout) != nrow(x)) {
		yout <- approx(x$time, x$temp, xout, method = "linear", rule = 1)$y
		x <- tibble(time = xout, temp = yout, type = str_c("ref", i), type2 = "ref")
	}
	ref$t[[i]] <- x
}
ref  <- do.call(rbind, ref$t)
ref2 <- spread(ref, type, temp)

# get water temperature from loggers
tid <- fes.tides(loc, c(ymd_h(t0, t1)), dt)
tid$hi <- rollapply(tid$tide, 5, function(x) which.max(x) == 3, fill = FALSE)
tid <- filter(tid, hi)$time

wat <- filter(ref2, time %in% tid) %>%
	select(-time, -type2) %>%
	apply(1, mean) %>%
	tibble(time = tid, temp = .)

# run (or load) all lsm versions
if (del) file.remove("lsm_r_v10/t.r_v10.RData")
run_lsm("r_v10")
if (del) file.remove("lsm_fortran_v203/t.fortran_v203.RData")
run_lsm("fortran_v203")
file.remove("lsm_fortran_v191mod/t.fortran_v191mod.RData")
run_lsm("fortran_v191mod")

# merge values
lsm  <- do.call(rbind, list(lsm_r_v10, lsm_fortran_v203, lsm_fortran_v191mod, ref))
lsm1 <- lsm
#lsm1 <- filter(lsm1, type != "r_v10")
lsm2 <- filter(lsm1, time < ymd_h("2017-10-15 00"))
lsm3 <- filter(lsm1, time > ymd_h("2017-10-16 00") & time < ymd_h("2017-10-25 00"))

ggplot() +
	geom_line(data = filter(lsm1, type2 == "ref"), aes(time, temp, group = type), color = "darkgrey") +
	geom_line(data = filter(lsm1, type2 != "ref"), aes(time, temp, color = type)) +
	xlab("") + ylab("") + theme(legend.position = "top")
ggplot() +
	geom_line(data = filter(lsm2, type2 == "ref"), aes(time, temp, group = type), color = "darkgrey") +
	geom_line(data = filter(lsm2, type2 != "ref"), aes(time, temp, color = type)) +
	xlab("") + ylab("") + theme(legend.position = "top")
ggplot() +
	geom_line(data = filter(lsm3, type2 == "ref"), aes(time, temp, group = type), color = "darkgrey") +
	geom_line(data = filter(lsm3, type2 != "ref"), aes(time, temp, color = type)) +
	xlab("") + ylab("") + theme(legend.position = "top")



