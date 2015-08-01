# setwd("C:\\Users\\aaronf\\Documents\\classes\\data_science\\various\\wharf2wharf")
library(dplyr)
library(ggplot2)
library(gridExtra)
source("w2w_utils.R")

allData <- getData(2015)

# Get rid of some outliers - age = 0, time > 2.75 hours
allData <- dplyr::filter(allData, age > 0 & elapsed < 2.75 * 3600 * 1000)

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

yticks <- seq(0, max(allData$elapsed), 900000)
plot <-
  ggplot(allData, aes(x = age, y = elapsed, color=sex)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) + 
  scale_y_continuous(breaks = yticks, labels = timestr(yticks), name = "elapsed time (hh:mm:ss)") + 
  geom_point() +
  expand_limits(y = 0.25 * 3600 * 1000) +
  stat_smooth(formula = y~x) +
  geom_point(data=friends,aes(x = age, y = elapsed, shape=lastname), color = "black")
grid.arrange(plot, nrow=1)

