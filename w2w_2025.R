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

# same fixed hues as w2w_2026.R's start_plot, so sex colors are consistent
# year over year
sex_colors <- c(Female="#F8766D", Male="#00BFC4", `Non-binary`="#7CAE00", Unknown="#C77CFF")

start_plot <- ggplot(allData, aes(x = elapsed, y = start, color = sex)) +
  scale_y_continuous(breaks = start_ticks, labels = timestr(start_ticks)) +
  scale_x_continuous(breaks = elapsed_ticks, labels = timestr(elapsed_ticks)) +
  scale_color_manual(values = sex_colors) +
  expand_limits(x = 0.25 * 3600 * 1000, y = 8.5 * 3600 * 1000) +
  geom_point() +
  ggtitle("Wharf to Wharf 2025")

png(filename="two_plots.png", width=1024, height=794)
grid.arrange(elapsed_plot, start_plot, nrow=2)
dev.off()

svg(filename="start_plot.svg", width=10, height=9)
print(start_plot)
dev.off()

png(filename="start_plot.png", width=1024, height=794)
print(start_plot)
dev.off()

# note: no "early starts" plot here -- since 2022, w2w_utils.R reconstructs
# `start` as 8:00 + (gunTime - chipTime), which can never be earlier than
# 8:00 by construction, so an "earlyStarts" subset is always empty for this
# data era. That analysis only made sense in years with real per-runner
# clock-start timestamps (e.g. w2w_2018.R).
