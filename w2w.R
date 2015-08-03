# setwd("C:\\Users\\aaronf\\Documents\\classes\\data_science\\various\\wharf2wharf")
library(dplyr)
library(ggplot2)
library(gridExtra)
source("w2w_utils.R")

allData <- getData(2015)

# Get rid of some outliers - age = 0, time > 2.75 hours
allData <- dplyr::filter(allData, age > 0 & elapsed < 2.75 * 3600 * 1000)
# ... 35 records have start time before 8:30, with suspicious values like 7:30,
# 8:00, 8:15, 8:29. I don't see any obvious pattern for these runners. Dropping
# them all because they cloud the corral picture.
allData <- dplyr::filter(allData, start > 30090000)

# Display some people's data differently.
if (file.exists("friends_priv.R")) {
  source("friends_priv.R")
  friends <- getFriends(allData)
} else {
  friends <- subset(allData,
    lastname == "FERRUCCI"
  )
}
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
