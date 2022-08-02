library(dplyr)
library(ggplot2)
library(gridExtra)
source("w2w_utils.R")

allData <- getData(2022)

# Some age values are NA.
# Drop them.
allData <- dplyr::filter(allData, age > 0)

# About 4000 records have elapsed == NA. Maybe these are "virtual" runners?
# Drop them.
allData <- dplyr::filter(allData, !is.na(elapsed))

# Display some people's data differently.
if (file.exists("friends_priv.R")) {
  source("friends_priv.R")
  friends <- getFriends(allData)
} else {
  friends <- subset(allData,
    name == "<your name here>"
  )
  friends$name = factor(friends$name)
}

elapsed_ticks <- seq(0, max(allData$elapsed) + extract_elapsed("0:14:59"), extract_elapsed("0:15:00"))
elapsed_plot <-
  ggplot(allData, aes(x = age, y = elapsed, color=sex)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) + 
  scale_y_continuous(breaks = elapsed_ticks, labels = timestr(elapsed_ticks), name = "elapsed time (hh:mm:ss)") + 
  geom_point() +
  expand_limits(y = 0.25 * 3600 * 1000) +
  geom_smooth(method=loess, formula = y ~ x) +
  geom_point(data=friends,aes(x = age, y = elapsed, shape=name), color = "black")

start_ticks <- seq(8.5 * 3600 * 1000, max(allData$start), 0.0625 * 3600 * 1000)
start_plot <- ggplot(allData, aes(x = elapsed, y = start, color = sex)) + 
  scale_y_continuous(breaks = start_ticks, labels = timestr(start_ticks)) + 
  scale_x_continuous(breaks = elapsed_ticks, labels = timestr(elapsed_ticks)) +
  expand_limits(x = 0.25 * 3600 * 1000, y = 8.5 * 3600 * 1000) +
  geom_point() +
  geom_point(data=friends,aes(x = elapsed, y = start, shape=name), color = "black")

grid.arrange(elapsed_plot, start_plot, nrow=2)
