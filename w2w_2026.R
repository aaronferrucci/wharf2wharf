library(dplyr)
library(ggplot2)
source("w2w_utils.R")

clean <- function(year, allData) {
  if (year != 2026) {
    stop(paste0("No support for year: ", year))
  }

  # Drop records with elapsed > 3 hours, 30 minutes
  allData <- dplyr::filter(allData, elapsed < 2.75 * 3600 * 1000)
  # Drop records with elapsed == 0
  allData <- dplyr::filter(allData, elapsed > 0)

  allData$age = as.numeric(allData$age)

  return(allData);
}

allData <- getData(2026)
allData <- clean(2026, allData)

elapsed_ticks <- seq(0, max(allData$elapsed), 900000)
start_ticks <- seq(8 * 3600 * 1000, max(allData$start), 0.0625 * 3600 * 1000)

start_plot <- ggplot(allData, aes(x = elapsed, y = start, color = age)) +
  scale_y_continuous(breaks = start_ticks, labels = timestr(start_ticks)) +
  scale_x_continuous(breaks = elapsed_ticks, labels = timestr(elapsed_ticks)) +
  expand_limits(x = 0.25 * 3600 * 1000, y = 8.5 * 3600 * 1000) +
  geom_point()

svg(filename="start_plot.svg", width=10, height=9)
print(start_plot)
dev.off()

png(filename="start_plot.png", width=1024, height=794)
print(start_plot)
dev.off()
