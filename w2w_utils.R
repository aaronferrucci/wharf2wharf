library(jsonlite)
library(RCurl)
library(dplyr) #for debugging

path_to_data <- "."

stripJQ <- function(str) {
        str <- sub("^jQuery.*?\\(", "", str, perl=TRUE);
        str <- sub("\\);$", "", str, perl=TRUE);
        allData <- fromJSON(str)

        return(allData)
}

# 2022 queries look like this:
# https://results.raceroster.com/v2/en-US/results/77rcx5a3p6amwxyh/results?subEvent=138514&page=1&pageSize=500
# there seems to be no way to do a "size 1" query (which I used to do to get the total number of records, before the
# main data-capture loop). No problem, I can grab the first 500 and total number at the same time, then proceed to the loop.
getQuery <- function(start, limit, year) {
  if (year != 2022) {
    stop(paste0("No support for year: ", year))
  }
  fmt <- "https://results.raceroster.com/v2/api/result-events/33098/sub-events/141035/results?start=%d&limit=%d&locale=en-US&associationUuid=9eec16e6-fa5b-11e8-9bc5-0e03b32fca98"
  return(sprintf(fmt, start, limit))
}


# compute elapsed chipTime in ms
extract_elapsed <- function(data0) {
  nums <- lapply(strsplit(data0$chipTime, ':', fixed=T), as.integer)

  to_ms <- function(x) 1000 * sum(c(60, 1) * unlist(x))
  elapsed <- sapply(nums, to_ms)

  return(elapsed)
}

# In the 2017 data, many entries are missing their startTime (data0$start$time_ms).
# The problem is in the raw data. What to do? Fortunately, there's some redundancy in
# the data: there are a number of "splits" (taken at 2 mile intervals, I think), and
# each split has a current time (time_ms) and an elapsed time since the start (elapsed).
# From those values, the start time can be calculated (time_ms - elapsed). This function
# iterates over the splits, choosing the first non-zero calculated start time.
calculate_start_via_2nd_split <- function(data0) {
  # Start with zero values, and overwrite with non-zero below.  
  start <- rep(0, nrow(data0))

  splits <- data0$splits
  num_names <- length(names(splits))
  for (split in 1:num_names) {
    name <- names(splits[split])
    start <- ifelse(start > 0, start, splits[[name]]$time_ms - splits[[name]]$elapsed)
  }
  return(start)
}

timestr <- function(elapsed) {
        # elapsed is in ms, convert to s
        seconds <- elapsed / 1000.0
        hours <- as.integer(seconds / 3600)
        seconds <- seconds - hours * 3600
        minutes <- as.integer(seconds / 60)
        seconds <- round(seconds - minutes * 60, digits=2)

        minute_prefix <- ifelse(minutes < 10, "0", "")
        minutes <- paste0(minute_prefix, minutes)
        second_prefix <- ifelse(seconds < 10, "0", "")
        seconds <- paste0(second_prefix, seconds)

        time <- paste(hours, minutes, seconds, sep=":")
        return(time)
}

# Get race data from the web site or from a local cache file.
getData <- function(year) {
  if (year != 2022) {
    stop(paste0("No support for year: ", year))
  }

        filename <- paste0(path_to_data, "/", "w2w", year, ".csv")
        force = FALSE
        if (!force & file.exists(filename)) {
                allData <- read.csv(filename, stringsAsFactors = FALSE)
        } else {
                # get first page of data
                url <- getQuery(0, 50, year)
                p0 <- getURL(url)
                allData <- stripJQ(p0)
                totalRecords <- allData$meta$totalResults
                print(sprintf("totalRecords: %d", totalRecords))

                allData <- data.frame()

                tags <- c("name", "bib", "fromCity", "age", "genderSexId", "overallPlace", "genderPlace", "divisionPlace", "chipTime", "gunTime")
                start <- 0
                size <- 100
                while (start < totalRecords) {
                        length <- min(size, totalRecords - start)
                        print(sprintf("start, length: %d, %d", start, length))
                        url <- getQuery(start, length, year)
                        p0 <- getURL(url)
                        thisData <- stripJQ(p0)
                        data0 <- thisData$data
                        data <- data0[,tags]

                        data$elapsed <- extract_elapsed(data0)
                        data$elapsedTime <- timestr(data$elapsed)
                        # In some cases, start time is not available, but can be calculated from split data.
                        calculatedStart <- calculate_start_via_2nd_split(data0)
                        # Prefer $start$time_ms, fall back on calculated start time.
                        # Possibly I can compute start time as
                        # 8:30 + (gunTime - chipTime)
                        # depends on whether everyone's 'gunTime' started right
                        # at 8:30, or if they had different "guns" per corral.
                        # In fact I heard no guns at all.
                        data$start <- ifelse(data0$start$time_ms > 0, data0$start$time_ms, calculatedStart)
                        data$startTime <- timestr(data$start)

                        allData <- bind_rows(allData, data)
                        start <- start + length
                }

                write.csv(allData, filename)
        }

        return(allData)
}

#
# Clean the data, as is done in w2w.Rmd
#
clean <- function(year, allData) {
        if (year == 2015) {
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

# In cases where a record's "age" is 0, but a record with the same name has non-zero age in a previous year,
# calculate their current age as 1 year greater than that of the previous year's data.
#
# Bugs:
#   A name match doesn't mean it's actually the same person
#   The person may not actually be 1 year older - if their birthday was after a previous year's run, but
#     before this year's, say.
imputeAgeFromOldData <- function(allData, year, oldYear) {
        dataOld <- getCleanData(oldYear)
        age0 <- allData[allData$age == 0,]
        ageOld <- merge(age0, dataOld, by=c("firstname", "lastname", "sex"))
        for (i in 1:nrow(ageOld)) {
          # Update age0 with non-zero older data
          if (ageOld[i,]$age.y != 0) {
            age0[age0$firstname == ageOld[i,]$firstname & age0$lastname == ageOld[i,]$lastname,]$age = ageOld[i,]$age.y + year - oldYear
          }
        }
        print(paste0("imputeAgeFromOldData(", year, ", ", oldYear, "): found ", nrow(age0[age0$age > 0,]), " age records to impute"))
        # Assign age0 data to the relevant rows in allData
        allData[allData$age==0, "age"] <- age0$age
        return(allData)
}

# get and clean.
getCleanData <- function(year) {
        return(clean(year, getData(year)))
}

# For WharfToWharfR, remove the user names.
anonymize <- function(data) {
        data <- subset(data, select = -c(firstname, lastname))
        return(data)
}
