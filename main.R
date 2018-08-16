Sys.setenv(TZ = "UTC")
options(digits = 10)
library(tidyverse)
library(lubridate)
library(stringr)
library(xts)
library(dygraphs)
library(scales)
library(shiny)

del <- FALSE
# del <- TRUE

DIR <- str_c(dirname(getwd()), "/io/")
source("functions.R")

t0  <- "2017-10-10 00"
t1  <- "2018-03-01 00"
# t1  <- "2017-12-01 00"
dt  <- 1800
loc <- list(lon = -8.876, lat = 41.839, height = -20)

# robolimpet data ####
ref <- tibble(
	path = dir(str_c(DIR, "robolimpet/"), full.names = TRUE, pattern = "msu"))
ref$t <- as.list(rep(NA, nrow(ref)))

for (i in 1:nrow(ref)) {
	x <- suppressMessages(read_csv(ref$path[i], col_names = c("time", "temp"), skip = 2))
	# trim xout data to match the T_RANGE used in the LSMs
	xout <- seq(max(first(x$time), ymd_h(t0)), min(last(x$time), ymd_h(t1)), dt)
	if (length(xout) != nrow(x)) {
		yout <- approx(x$time, x$temp, xout, method = "linear", rule = 1)$y
		x <- tibble(time = xout, temp = yout, type = str_c("ref", i))
	}
	ref$t[[i]] <- x
}
ref  <- do.call(rbind, ref$t)
ref2 <- spread(ref, type, temp)
REF  <- xts(select(ref2, -time), ref2$time)

# water temperature  ####
# ... from loggers
tid <- fes.tides(loc, c(ymd_h(t0, t1)), dt)
tid$hi <- rollapply(tid$tide, 5, function(x) which.max(x) == 3, fill = FALSE)
tid <- filter(tid, hi)$time

wat <- filter(ref2, time %in% tid) %>%
	select(-time) %>%
	apply(1, mean) %>%
	xts(tid)
colnames(wat) <- "sst"

# load "w" - for use if "r_v10" is not run again
w <- forcing.data.main()

# run LSMs ####
# (or load)
if (del) file.remove("lsm_r_v10/t.r_v10.RData")
run_lsm("r_v10")
if (del) file.remove("lsm_fortran_v203/t.fortran_v203.RData")
run_lsm("fortran_v203")
file.remove("lsm_fortran_v191mod/t.fortran_v191mod.RData")
run_lsm("fortran_v191mod")

# prepare data ####
# ... for shiny
OUT  <- cbind(lsm_r_v10, lsm_fortran_v203, lsm_fortran_v191mod, REF$ref1, REF$ref2)
col1 <- c("lsm_r_v10", "lsm_fortran_v203", "lsm_fortran_v191mod", "ref1", "ref2")
colnames(OUT) <- col1
color1 <- set_names(c("orange", "green", "red", "darkgrey", "darkgrey"), col1)

W <- apply(w, 2, function(x) rescale(x, range(OUT)))
col2 <- colnames(W)
OUT2 <- cbind(coredata(OUT), W)

# visualize ####
ui <- fluidPage(
	fluidRow(
		column(2,
					 checkboxInput(inputId  = "ylim", label = strong("fixed ylim"), value = FALSE),
					 hr(),
					 checkboxGroupInput(inputId  = "col1", label = strong("body temp"),
					 									 choices  = col1, selected = col1),
					 hr(),
					 radioButtons(inputId  = "col2", label = strong("env"), choices = col2, selected = col2[1])),
		mainPanel(
			fluidRow(
				column(9, dygraphOutput("plot", height = "700px")),
				column(3,  textOutput("legendDivID")))))
)

server <- function(input, output) {
	# Subset data
	subset.data <- reactive({
		col <- as.character(color1[input$col1])
		df  <- OUT2[, c(input$col1, input$col2)]
		list(df = df, col = col)
	})

	output$plot <- renderDygraph({
		dat <- subset.data()
		d <- dygraph(dat$df) %>%
			dyRangeSelector(dateWindow = c(ymd_h(t0), ymd_h(t0) + (6 * 24 * 3600))) %>%
			dyLegend(
				show = "always",
				hideOnMouseOut = FALSE,
				labelsSeparateLines = TRUE,
				labelsDiv = "legendDivID") %>%
			dyOptions(
				retainDateWindow = TRUE,
				colors = c(dat$col, "blue"))
		if (input$ylim) d <- d %>% dyAxis("y", valueRange = range(OUT))
		d
	})
}

shinyApp(ui = ui, server = server)
