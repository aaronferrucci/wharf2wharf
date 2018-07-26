library(dplyr)
library(ggplot2)
library(gridExtra)
source("w2w_utils.R")

allData <- getData(2018)
earlyStarts <- allData[allData$start < 8 * 3600 * 1000,]
noAge <- allData[allData$age == 0,]

allData <- clean(2018, allData)

# Display some people's data differently.
friends <- subset(allData, lastname == "FERRUCCI" & firstname == "AARON")
friends$lastname = factor(friends$lastname)

elapsed_ticks <- seq(0, max(allData$elapsed), 900000)
elapsed_plot <-
  ggplot(allData, aes(x = age, y = elapsed, color=sex)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) + 
  scale_y_continuous(breaks = elapsed_ticks, labels = timestr(elapsed_ticks), name = "elapsed time (hh:mm:ss)") + 
  geom_point() +
  expand_limits(y = 0.25 * 3600 * 1000) +
  stat_smooth(formula = y~x) +
  geom_point(data=friends,aes(x = age, y = elapsed, shape=lastname), color = "black")

start_ticks <- seq(8.5 * 3600 * 1000, max(allData$start), 0.0625 * 3600 * 1000)
start_plot <- ggplot(allData, aes(x = elapsed, y = start, color = sex)) + 
  scale_y_continuous(breaks = start_ticks, labels = timestr(start_ticks)) + 
  scale_x_continuous(breaks = elapsed_ticks, labels = timestr(elapsed_ticks)) +
  expand_limits(x = 0.25 * 3600 * 1000, y = 8.5 * 3600 * 1000) +
  geom_point() +
  geom_point(data=friends,aes(x = elapsed, y = start, shape=lastname), color = "black")

grid.arrange(elapsed_plot, start_plot, nrow=2)

early_ticks <- seq(0, max(earlyStarts$elapsed), 900000)
early_plot <-
  ggplot(earlyStarts, aes(x = age, y = elapsed, color=sex)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) + 
  scale_y_continuous(breaks = early_ticks, labels = timestr(early_ticks), name = "elapsed time (hh:mm:ss)") + 
  geom_point() +
  expand_limits(y = 0.25 * 3600 * 1000) +
  stat_smooth(formula = y~x)
