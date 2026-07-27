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

# w2w2026_raw.csv is already sorted by overallPlace, but sort explicitly
# here too -- geom_point() draws in row order with opaque points, so a
# future re-scrape that doesn't preserve that order (e.g. grouped by sex,
# as w2w2025_raw.csv originally was) would silently bias the plot again.
allData <- allData[order(allData$overallPlace), ]

elapsed_ticks <- seq(0, max(allData$elapsed), 900000)
start_ticks <- seq(8 * 3600 * 1000, max(allData$start), 0.0625 * 3600 * 1000)

# Female/Male keep the exact hues from the 2025 plot's default 2-color scale
# (scales::hue_pal()(2)); Non-binary/Unknown are new this year, so they get
# the other two slots from that same palette's 4-color step
# (scales::hue_pal()(4)) rather than colors that could collide.
sex_colors <- c(Female="#F8766D", Male="#00BFC4", `Non-binary`="#7CAE00", Unknown="#C77CFF")

start_plot <- ggplot(allData, aes(x = elapsed, y = start, color = sex)) +
  scale_y_continuous(breaks = start_ticks, labels = timestr(start_ticks)) +
  scale_x_continuous(breaks = elapsed_ticks, labels = timestr(elapsed_ticks)) +
  scale_color_manual(values = sex_colors) +
  expand_limits(x = 0.25 * 3600 * 1000, y = 8.5 * 3600 * 1000) +
  geom_point() +
  ggtitle("Wharf to Wharf 2026")

svg(filename="start_plot.svg", width=10, height=9)
print(start_plot)
dev.off()

png(filename="start_plot.png", width=1024, height=794)
print(start_plot)
dev.off()
