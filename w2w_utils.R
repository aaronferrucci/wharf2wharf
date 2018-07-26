library(jsonlite)
library(RCurl)
library(dplyr) #for debugging
stripJQ <- function(str) {
        str <- sub("^jQuery.*?\\(", "", str, perl=TRUE);
        str <- sub("\\);$", "", str, perl=TRUE);
        allData <- fromJSON(str)

        return(allData)
}

getQuery <- function(start, length, year) {
        if (year == 2015) {
                fmt <- "http://results.xacte.com/json/agegroup?eventId=1009&subeventId=2365&categoryId=2174&sex=-1&agegroupId=-1&tagcode=undefined&callback=jQuery18308243213289018978_1438222582599&sEcho=12&iColumns=11&iDisplayStart=%d&iDisplayLength=%d&mDataProp_1=bib&mDataProp_2=firstname&mDataProp_3=city&mDataProp_4=sex&mDataProp_8=overall&mDataProp_9=oversex&mDataProp_10=overdiv&bRegex=false&bRegex_0=false&bSearchable_0=false&bRegex_1=false&bSearchable_1=true&bRegex_2=false&bSearchable_2=true&bRegex_3=false&bSearchable_3=true&bRegex_4=false&bSearchable_4=true&bRegex_5=false&bSearchable_5=true&bRegex_6=false&bSearchable_6=true&bRegex_7=false&bSearchable_7=true&bRegex_8=false&bSearchable_8=true&bRegex_9=false&bSearchable_9=true&bRegex_10=false&bSearchable_10=true&iSortingCols=0&bSortable_0=false&bSortable_1=false&bSortable_2=false&bSortable_3=false&bSortable_4=false&bSortable_5=false&bSortable_6=false&bSortable_7=false&bSortable_8=false&bSortable_9=false&bSortable_10=false&_=1438224723854"
        }

        if (year == 2016) {
                fmt <- "http://results.xacte.com/json/agegroup?eventId=1294&subeventId=3043&categoryId=3169&sex=-1&agegroupId=-1&tagcode=undefined&callback=jQuery1830027340891935842104_1472607831773&sEcho=8&iColumns=11&sColumns=&iDisplayStart=%d&iDisplayLength=%d&mDataProp_0=&mDataProp_1=bib&mDataProp_2=firstname&mDataProp_3=city&mDataProp_4=sex&mDataProp_5=&mDataProp_6=&mDataProp_7=&mDataProp_8=overall&mDataProp_9=oversex&mDataProp_10=overdiv&sSearch=&bRegex=false&sSearch_0=&bRegex_0=false&bSearchable_0=false&sSearch_1=&bRegex_1=false&bSearchable_1=true&sSearch_2=&bRegex_2=false&bSearchable_2=true&sSearch_3=&bRegex_3=false&bSearchable_3=true&sSearch_4=&bRegex_4=false&bSearchable_4=true&sSearch_5=&bRegex_5=false&bSearchable_5=true&sSearch_6=&bRegex_6=false&bSearchable_6=true&sSearch_7=&bRegex_7=false&bSearchable_7=true&sSearch_8=&bRegex_8=false&bSearchable_8=true&sSearch_9=&bRegex_9=false&bSearchable_9=true&sSearch_10=&bRegex_10=false&bSearchable_10=true&iSortingCols=0&bSortable_0=false&bSortable_1=false&bSortable_2=false&bSortable_3=false&bSortable_4=false&bSortable_5=false&bSortable_6=false&bSortable_7=false&bSortable_8=false&bSortable_9=false&bSortable_10=false&_=1472607958000"
        }

        if (year == 2017) {
                fmt <- "http://results.xacte.com/json/agegroup?eventId=1477&subeventId=3886&categoryId=6283&sex=-1&agegroupId=-1&tagcode=undefined&callback=jQuery183043211056341577336_1500931768127&sEcho=1&iColumns=11&sColumns=&iDisplayStart=%d&iDisplayLength=%d&mDataProp_0=&mDataProp_1=bib&mDataProp_2=firstname&mDataProp_3=city&mDataProp_4=sex&mDataProp_5=&mDataProp_6=&mDataProp_7=&mDataProp_8=overall&mDataProp_9=oversex&mDataProp_10=overdiv&sSearch=&bRegex=false&sSearch_0=&bRegex_0=false&bSearchable_0=false&sSearch_1=&bRegex_1=false&bSearchable_1=true&sSearch_2=&bRegex_2=false&bSearchable_2=true&sSearch_3=&bRegex_3=false&bSearchable_3=true&sSearch_4=&bRegex_4=false&bSearchable_4=true&sSearch_5=&bRegex_5=false&bSearchable_5=true&sSearch_6=&bRegex_6=false&bSearchable_6=true&sSearch_7=&bRegex_7=false&bSearchable_7=true&sSearch_8=&bRegex_8=false&bSearchable_8=true&sSearch_9=&bRegex_9=false&bSearchable_9=true&sSearch_10=&bRegex_10=false&bSearchable_10=true&iSortingCols=0&bSortable_0=false&bSortable_1=false&bSortable_2=false&bSortable_3=false&bSortable_4=false&bSortable_5=false&bSortable_6=false&bSortable_7=false&bSortable_8=false&bSortable_9=false&bSortable_10=false&_=1500931825246"
        }
        if (year == 2018) {
                fmt <- "http://results.xacte.com/json/agegroup?eventId=2078&subeventId=5105&categoryId=7926&sex=-1&agegroupId=-1&tagcode=undefined&callback=jQuery183016611066151815979_1532530544207&sEcho=1&iColumns=11&sColumns=&iDisplayStart=%d&iDisplayLength=%d&mDataProp_0=&mDataProp_1=bib&mDataProp_2=firstname&mDataProp_3=city&mDataProp_4=sex&mDataProp_5=&mDataProp_6=&mDataProp_7=&mDataProp_8=overall&mDataProp_9=oversex&mDataProp_10=overdiv&sSearch=&bRegex=false&sSearch_0=&bRegex_0=false&bSearchable_0=false&sSearch_1=&bRegex_1=false&bSearchable_1=true&sSearch_2=&bRegex_2=false&bSearchable_2=true&sSearch_3=&bRegex_3=false&bSearchable_3=true&sSearch_4=&bRegex_4=false&bSearchable_4=true&sSearch_5=&bRegex_5=false&bSearchable_5=true&sSearch_6=&bRegex_6=false&bSearchable_6=true&sSearch_7=&bRegex_7=false&bSearchable_7=true&sSearch_8=&bRegex_8=false&bSearchable_8=true&sSearch_9=&bRegex_9=false&bSearchable_9=true&sSearch_10=&bRegex_10=false&bSearchable_10=true&iSortingCols=0&bSortable_0=false&bSortable_1=false&bSortable_2=false&bSortable_3=false&bSortable_4=false&bSortable_5=false&bSortable_6=false&bSortable_7=false&bSortable_8=false&bSortable_9=false&bSortable_10=false&_=1532530544744";
        }
        return(sprintf(fmt, start, length))
}


# The elapsed-time (in ms) for runners is buried inside
# some sort of hierarchical data structure. This routine tries
# to extract that elapsed time data, in a very ad-hoc manner. The
# routine works for 2015 and 2016; for other years it's untested.
extract_elapsed <- function(data0) {
        splits <- data0$splits
        num_names <- length(names(splits))
        last_name <- names(splits)[num_names]
        last_split <- splits[[last_name]]
        elapsed <- last_split$elapsed

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
        filename <- paste0("w2w", year, ".csv")
        force = FALSE
        if (!force & file.exists(filename)) {
                allData <- read.csv(filename, stringsAsFactors = FALSE)
        } else {
                # Just get total records
                url <- getQuery(0, 1, year)
                p0 <- getURL(url)
                allData <- stripJQ(p0)
                totalRecords <- allData$iTotalRecords
                print(sprintf("totalRecords: %d", totalRecords))
                data0 <- allData$aaData

                allData <- data.frame()

                tags <- c("firstname", "lastname", "bib", "city", "state", "country", "age", "sex", "overall", "oversex", "overdiv")
                start <- 0
                size <- 200
                while (start < totalRecords) {
                        length <- min(size, totalRecords - start)
                        print(sprintf("start, length: %d, %d", start, length))
                        url <- getQuery(start, length, year)
                        p0 <- getURL(url)
                        thisData <- stripJQ(p0)
                        data0 <- thisData$aaData
                        data <- data0[,tags]

                        # data$elapsed <- data0$splits$mysterious_seperator$elapsed
                        data$elapsed <- extract_elapsed(data0)
                        data$elapsedTime <- timestr(data$elapsed)
                        # In some cases, start time is not available, but can be calculated from split data.
                        calculatedStart <- calculate_start_via_2nd_split(data0)
                        # Prefer $start$time_ms, fall back on calculated start time.
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
