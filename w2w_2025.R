library(dplyr)
library(ggplot2)
library(gridExtra)
source("w2w_utils.R")

#
# Clean the data, as is done in w2w.Rmd
#
clean <- function(year, allData) {
        if (year == 2025) {
          # Drop records with elapsed > 3 hours, 30 minutes
          allData <- dplyr::filter(allData, elapsed < 2.75 * 3600 * 1000)
          # Drop records with elapsed == 0          
          allData <- dplyr::filter(allData, elapsed > 0)
          
          allData$age = as.numeric(allData$age)
        } else if (year == 2015) {
                # UMI? typo? I think they mean USA
                allData[allData$country == "UMI", c("country")] <- c("USA")

                # Three records mixed up country and city assignments (I guess).
                allData[allData$country == "", c("country")] <- allData[allData$country == "", c("city")]
                allData[grepl("^KEN", allData$country), c("country")] <- c("KEN")
                allData[grepl("^ERI", allData$country), c("country")] <- c("ERI")

                # Drop entrants 4 and younger (stroller participants?)
                allData <- dplyr::filter(allData, age >= 5)

                # Drop records with elapsed > 3 hours, 30 minutes
                allData <- dplyr::filter(allData, elapsed < 2.75 * 3600 * 1000)

                # Drop ridiculous records that started before the race start time (8:30AM)
                allData <- dplyr::filter(allData, start > 30090000)
        } else if (year == 2017) {
                # Annoyingly, in 2017, "USA" becomes "US". Make it backward-compatible
                allData[allData$country == "US", c("country")] <- c("USA")

                # I'd drop entrants with age 4 and lower, but this year there a large number of real-looking
                # records with age=0.

                # 1 participants has country == "", but state == TX. Country is "USA"
                allData[allData$country == "" & allData$state == "TX", c("country")] <- c("USA")
                # Likewise for 1 participant with state == CA
                allData[allData$country == "" & allData$state == "CA", c("country")] <- c("USA")
                # 2 participants have blank country, state and city. I'll guess the most likely country and state.
                bibs <- dplyr::filter(allData, state == "" & country == "")$bib
                allData[allData$bib %in% bibs,]$state <- c("CA")
                allData[allData$bib %in% bibs,]$country <- c("USA")

                # This year names are a mix of upper and lower case. Make it all uppercase, like previous years.
                allData$firstname <- toupper(allData$firstname)
                allData$lastname <- toupper(allData$lastname)

                # For countries KE, ET, translate to canonical "KEN", "ETH".
                allData[allData$country == "KE", c("country")] <- c("KEN")
                allData[allData$country == "ET", c("country")] <- c("ETH")

                # Fill in missing age data from previous years
                allData <- imputeAgeFromOldData(allData, year, 2015)
                allData <- imputeAgeFromOldData(allData, year, 2016)
        } else if (year == 2018) {
                # There are tens of records with start time exactly 7:30, with very long elapsed times - over 1.5 hours, many over 2 hours.
                # I don't know who these people are - maybe wheelchair? Omit them.
                allData <- dplyr::filter(allData, start > (8 * 3600 * 1000))
          
                # Fix country errors
                # "USA" for backward compatibility
                allData[allData$country == "US", c("country")] <- c("USA")
                # "KEN" for backward compatibility
                allData[allData$country == "KE", c("country")] <- c("KEN")
                
                # A handful of country typos (?), all determined to be USA by city and state name.
                allData[allData$country == "UM", c("country")] <- c("USA")
                allData[allData$country == "AX", c("country")] <- c("USA")
                allData[allData$country == "MX", c("country")] <- c("USA")
                allData[allData$country == "CM", c("country")] <- c("USA")
                allData[allData$country == "" & allData$state == "CA", c("country")] <- c("USA")
                
                # Two records remain with country == "". One has a name which appears in previous w2w, with country="USA";
                # For the last one - just make a guess - pretty good odds - it's USA.
                allData[allData$country == "", c("country")] <- c("USA")
                
                # One record has sex == NA. Executive decision: assert a gender
                allData[allData$bib == 827,]$sex = "M"
             
                # Uppercase is the standard now
                allData$firstname <- toupper(allData$firstname)
                allData$lastname <- toupper(allData$lastname)
                
                # patch a few age-0 records using data from previous years
                allData <- imputeAgeFromOldData(allData, year, 2015)
                allData <- imputeAgeFromOldData(allData, year, 2016)
                allData <- imputeAgeFromOldData(allData, year, 2017)
        }

        return(allData);
}

allData <- getData(2025)

allData <- clean(2025, allData)
noAge <- allData[is.na(allData$age), ]

# Display some people's data differently.
friends <- subset(allData, name == "Aaron Ferrucci" | name == "Ian Ferrucci")
friends$name = factor(friends$name)

elapsed_ticks <- seq(0, max(allData$elapsed), 900000)
elapsed_plot <-
  ggplot(allData, aes(x = age, y = elapsed, color=sex)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) + 
  scale_y_continuous(breaks = elapsed_ticks, labels = timestr(elapsed_ticks), name = "elapsed time (hh:mm:ss)") + 
  geom_point() +
  expand_limits(y = 0.25 * 3600 * 1000) +
  stat_smooth(formula = y~x) +
  geom_point(data=friends,aes(x = age, y = elapsed, shape=name), color = "black")

start_ticks <- seq(8 * 3600 * 1000, max(allData$start), 0.0625 * 3600 * 1000)
start_plot <- ggplot(allData, aes(x = elapsed, y = start, color = sex)) + 
  scale_y_continuous(breaks = start_ticks, labels = timestr(start_ticks)) + 
  scale_x_continuous(breaks = elapsed_ticks, labels = timestr(elapsed_ticks)) +
  expand_limits(x = 0.25 * 3600 * 1000, y = 8.5 * 3600 * 1000) +
  geom_point() +
  geom_point(data=friends,aes(x = elapsed, y = start, shape=name), color = "black")

grid.arrange(elapsed_plot, start_plot, nrow=2)

early_ticks <- seq(0, max(earlyStarts$elapsed), 900000)
early_plot <-
  ggplot(earlyStarts, aes(x = age, y = elapsed, color=sex)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) + 
  scale_y_continuous(breaks = early_ticks, labels = timestr(early_ticks), name = "elapsed time (hh:mm:ss)") + 
  geom_point() +
  expand_limits(y = 0.25 * 3600 * 1000) +
  stat_smooth(formula = y~x)
