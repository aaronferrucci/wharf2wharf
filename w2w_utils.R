library(jsonlite)
library(RCurl)
library(dplyr) # for debugging

path_to_data <- "."

stripJQ <- function(str) {
  str <- sub("^jQuery.*?\\(", "", str, perl = TRUE)
  str <- sub("\\);$", "", str, perl = TRUE)
  allData <- fromJSON(str)

  return(allData)
}

getQuery <- function(startPage, limit, year) {
  # I tried to get this query to work, hit some errors. Fell back on manual
  # copy/paste from the results site.
  if (year != 2025) {
    stop(paste0("No support for year: ", year))
  }
  fmt <- "https://results.raceroster.com/v2/en-US/results/nphrbxhycu4efhpn/results?page=%d&pageSize=%d"
  return(sprintf(fmt, startPage, limit))
}

# convert h:mm:ss time to ms
extract_elapsed <- function(times) {
  # times are "mm:ss" or "h:mm:ss"
  # create list of lists of 2 or 3 elements
  splits <- strsplit(times, ":", fixed = TRUE)

  # prepend a "0" to the list, if the format was "mm:ss"
  prepend_if_2 <-
    function(x) as.integer(if (length(x) == 2) append(x, "0", after = 0) else x)
  nums <- lapply(splits, prepend_if_2)

  # convert to milliseconds
  to_ms <- function(x) 1000 * sum(c(3600, 60, 1) * unlist(x))
  elapsed <- sapply(nums, to_ms)

  return(elapsed)
}

timestr <- function(elapsed) {
  # elapsed is in ms, convert to s
  seconds <- elapsed / 1000.0
  hours <- as.integer(seconds / 3600)
  seconds <- seconds - hours * 3600
  minutes <- as.integer(seconds / 60)
  seconds <- round(seconds - minutes * 60, digits = 2)

  minute_prefix <- ifelse(minutes < 10, "0", "")
  minutes <- paste0(minute_prefix, minutes)
  second_prefix <- ifelse(seconds < 10, "0", "")
  seconds <- paste0(second_prefix, seconds)

  time <- paste(hours, minutes, seconds, sep = ":")
  return(time)
}

# 2022 genderPlace is of the form ' x / y'
# covert to 'x', as an integer
fixGenderPlace <- function(genderPlace) {
  splits <- strsplit(genderPlace, " ", fixed = TRUE)

  extract_place <- function(x) as.integer(x[1])
  nums <- lapply(splits, extract_place)

  return(nums)
}

# Get race data from the web site or from a local cache file.
getData <- function(year) {
  if (year != 2025) {
    stop(paste0("No support for year: ", year))
  }

  filename <- paste0(path_to_data, "/", "w2w", year, "_raw.csv")
  force <- FALSE
  if (!force && file.exists(filename)) {
    allData <- read.csv(filename, sep = '\t', stringsAsFactors = FALSE)
  } else {
    allData <- data.frame()
    totalRecords <- 0 # will be reassigned on the first capture below
    doneInit <- F
    start_page <- 1
    size <- 100

    while (!doneInit || start_page * size < totalRecords) {
      print(sprintf("start_page, size: %d, %d", start_page, size))
      url <- getQuery(start_page, size, year)
      print(url)
      p0 <- getURL(url)
      # thisData <- stripJQ(p0)
      data <- thisData$data

      # if this is the first capture, grab totalRecords
      if (!doneInit) {
        totalRecords <- thisData$meta$totalResults
        print(sprintf("totalRecords: %d", totalRecords))
        doneInit <- TRUE
      }

      # Some oddball records have blank overallPlace value, which forces the
      # field to class 'character'. Reverse that (blanks become integer N/A)
      data$overallPlace <- as.integer(data$overallPlace)
      allData <- bind_rows(allData, data)
      start <- start + size
    }
    write.csv(allData, filename)
  }

  # rename columns to match older names, so I don't have to change lots of
  # downstream code
  names(allData)[names(allData) == "Full.Name"] <- "name"
  names(allData)[names(allData) == "Bib"] <- "bib"
  names(allData)[names(allData) == "City"] <- "fromCity"
  names(allData)[names(allData) == "Age"] <- "age"
  names(allData)[names(allData) == "Gender"] <- "sex"
  names(allData)[names(allData) == "Race.Place"] <- "overallPlace"
  names(allData)[names(allData) == "Gender.Place"] <- "genderPlace"
  names(allData)[names(allData) == "Age.Group"] <- "division"
  names(allData)[names(allData) == "Age.Place"] <- "divisionPlace"
  names(allData)[names(allData) == "Chip.Elapsed.Time"] <- "chipTime"
  names(allData)[names(allData) == "Gun.Elapsed.Time"] <- "gunTime"
  names(allData)[names(allData) == "Overall.Pace"] <- "overallPace"

  tags <- c("name", "bib", "fromCity", "age", "sex", "overallPlace", "genderPlace", "divisionPlace", "chipTime", "gunTime", "overallPace")
  allData <- allData[, tags]

  allData$elapsed <- extract_elapsed(allData$chipTime)
  allData$elapsedTime <- timestr(allData$elapsed)

  # The 2022 race has "gunTime" and "chipTime" but (unlike previous years)
  # has no "start" time (time the corral started).
  # Experimentally, it looks like I can compute a start time as
  # 8:00 + (gunTime - chipTime)
  allData$start <- extract_elapsed(allData$gunTime)
  # gunTime - chipTime
  allData$start <- allData$start - allData$elapsed
  # add 8:00, so start is time of day (in ms)
  allData$start <- allData$start + ((8 * 60) + 0) * 60 * 1000
  allData$startTime <- timestr(allData$start)

  return(allData)
}

# For WharfToWharfR, remove the user names.
anonymize <- function(data) {
  data <- subset(data, select = -c(firstname, lastname))
  return(data)
}
