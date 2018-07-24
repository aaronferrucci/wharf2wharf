library(dplyr)
library(ggplot2)
library(gridExtra)
source("w2w_utils.R")

allData <- getData(2018)

# There are 32 of records with start time exactly 7:30, with very long elapsed times - over 1.5 hours, many over 2 hours.
# I don't know who these people are - maybe wheelchair? Omit them.
allData <- dplyr::filter(allData, start > (8 * 3600 * 1000))

# One record has sex == NA. Executive decision: assume a gender
allData[allData$bib == 827,]$sex = "M"

# Display some people's data differently.
friends <- subset(allData, lastname == "Ferrucci" & firstname == "Aaron")
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
