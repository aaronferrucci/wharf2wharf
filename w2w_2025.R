library(dplyr)
library(ggplot2)
library(gridExtra)
source("w2w_utils.R")

clean <- function(year, allData) {
  if (year != 2025) {
    stop(paste0("No support for year: ", year))
  }

  # Drop records with elapsed > 3 hours, 30 minutes
  allData <- dplyr::filter(allData, elapsed < 2.75 * 3600 * 1000)
  # Drop records with elapsed == 0
  allData <- dplyr::filter(allData, elapsed > 0)

  allData$age = as.numeric(allData$age)

  return(allData);
}

allData <- getData(2025)

allData <- clean(2025, allData)
noAge <- allData[is.na(allData$age), ]

elapsed_ticks <- seq(0, max(allData$elapsed), 900000)
elapsed_plot <-
  ggplot(allData, aes(x = age, y = elapsed, color=sex)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = elapsed_ticks, labels = timestr(elapsed_ticks), name = "elapsed time (hh:mm:ss)") +
  geom_point() +
  expand_limits(y = 0.25 * 3600 * 1000) +
  stat_smooth(formula = y~x)

start_ticks <- seq(8 * 3600 * 1000, max(allData$start), 0.0625 * 3600 * 1000)
start_plot <- ggplot(allData, aes(x = elapsed, y = start, color = age)) +
  scale_y_continuous(breaks = start_ticks, labels = timestr(start_ticks)) +
  scale_x_continuous(breaks = elapsed_ticks, labels = timestr(elapsed_ticks)) +
  expand_limits(x = 0.25 * 3600 * 1000, y = 8.5 * 3600 * 1000) +
  geom_point()

grid.arrange(elapsed_plot, start_plot, nrow=2)

early_ticks <- seq(0, max(earlyStarts$elapsed), 900000)
early_plot <-
  ggplot(earlyStarts, aes(x = age, y = elapsed, color=sex)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = early_ticks, labels = timestr(early_ticks), name = "elapsed time (hh:mm:ss)") +
  geom_point() +
  expand_limits(y = 0.25 * 3600 * 1000) +
  stat_smooth(formula = y~x)
