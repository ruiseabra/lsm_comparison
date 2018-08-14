Sys.setenv(TZ = "UTC")
options(digits = 10)
library(tidyverse)
library(lubridate)
library(stringr)
library(xts)
library(here)

wd <- "code/lsm_r_v10_v_fortran_v191/"
source(str_c(wd, "functions.R"))

t0  <- "2017-10-10 00"
t1  <- "2018-03-01 00"
dt  <- 1800
loc <- list(lon = -8.876, lat = 41.839, height = 30)

# run (or load) all lsm versions
run_lsm("r_v10")
run_lsm("fortran_v203")
run_lsm("fortran_v191mod")

# merge values
lsm1 <- do.call(rbind, list(lsm_r_v10, lsm_fortran_v203, lsm_fortran_v191mod))
lsm2 <- filter(lsm1, time < ymd_h("2017-10-15 00"))

ggplot(lsm2) +
  geom_line(aes(time, temp, color = type)) +
  xlab("") + ylab("")
#
#
#
#
#
# for (i in 1:nrow(temps)) {
#   loc$height <- temps$h[i]
#   temps$t.f[[i]] <- lsm.f(c(1), t0, t1, dt, loc) # lsm.f(c(1,3,5), t0, t1, dt, loc)
# }
#
# # merge data
# t <- list()
# for (i in 1:nrow(temps)) {
#   t[[i]] <- tibble(
#     nm   = temps$nm[i],
#     time = temps$ref[[i]]$time,
#     ref  = temps$ref[[i]]$ref_1,
#     lsmR = temps$t.r[[i]]$l1,
#     lsmF = temps$t.f[[i]]$l1)
# }
# t <- do.call(rbind, t)
# REF_mean <- mean(t$ref) - 3
# tr <- select(t, -lsmF) %>% cbind("R", .)
# tf <- select(t, -lsmR) %>% cbind("fortran", .)
# td <- select(t, -lsmR, -lsmF) %>% add_column(lsm = t$lsmR - t$lsmF + REF_mean) %>% cbind("R - fortran", .)
# colnames(tr) <- colnames(tf) <- colnames(td) <- c("type", "nm", "time", "ref", "lsm")
# t <- rbind(tr, tf, td) %>% as_tibble
# t$lsm <- round(t$lsm, 2)
#
# # plot all data
# tt <- filter(t, time > ymd_h("2017-11-01 00") & time < ymd_h("2017-11-20 00"))
# # ggplot(tt) +
# #   geom_line(aes(time, ref), col = "darkgrey", size = 1) +
# #   geom_line(aes(time, lsm), col = "blue") +
# #   facet_grid(nm ~ type) +
# #   xlab("") + ylab("") +
# #   theme(axis.title.x = element_blank(),
# #         axis.text.x  = element_blank(),
# #         axis.ticks.x = element_blank())
#
# bias_all  <- mean((td$lsm - REF_mean)) %>% round(3)
# bias_here <- mean((filter(tt, type == "R - fortran")$lsm - REF_mean)) %>% round(3)
# ggplot(tt) +
#   geom_hline(aes(yintercept = REF_mean)) +
#   geom_line(aes(time, lsm, color = type)) +
#   xlab("") + ylab("") + ggtitle(str_c("bias all: ", bias_all, " / bias here: ", bias_here))

