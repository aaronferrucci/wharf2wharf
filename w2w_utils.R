library(jsonlite)
library(RCurl)
stripJQ <- function(str) {
  str <- sub("^jQuery.*?\\(", "", str, perl=TRUE);
  str <- sub("\\);$", "", str, perl=TRUE);
  allData <- fromJSON(str)

  return(allData)
}

getQuery <- function(start, length) {
  fmt <- "http://results.xacte.com/json/agegroup?eventId=1009&subeventId=2365&categoryId=2174&sex=-1&agegroupId=-1&tagcode=undefined&callback=jQuery18308243213289018978_1438222582599&sEcho=12&iColumns=11&iDisplayStart=%d&iDisplayLength=%d&mDataProp_1=bib&mDataProp_2=firstname&mDataProp_3=city&mDataProp_4=sex&mDataProp_8=overall&mDataProp_9=oversex&mDataProp_10=overdiv&bRegex=false&bRegex_0=false&bSearchable_0=false&bRegex_1=false&bSearchable_1=true&bRegex_2=false&bSearchable_2=true&bRegex_3=false&bSearchable_3=true&bRegex_4=false&bSearchable_4=true&bRegex_5=false&bSearchable_5=true&bRegex_6=false&bSearchable_6=true&bRegex_7=false&bSearchable_7=true&bRegex_8=false&bSearchable_8=true&bRegex_9=false&bSearchable_9=true&bRegex_10=false&bSearchable_10=true&iSortingCols=0&bSortable_0=false&bSortable_1=false&bSortable_2=false&bSortable_3=false&bSortable_4=false&bSortable_5=false&bSortable_6=false&bSortable_7=false&bSortable_8=false&bSortable_9=false&bSortable_10=false&_=1438224723854" 

  return(sprintf(fmt, start, length))
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
  filename <- "w2w2015.csv"
  force = FALSE
  if (!force & file.exists(filename)) {
    allData <- read.csv(filename, stringsAsFactors = FALSE)
  } else {
    # Just get total records
    url <- getQuery(0, 1)
    p0 <- getURL(url)
    allData <- stripJQ(p0)
    totalRecords <- allData$iTotalRecords
    data0 <- allData$aaData

    allData <- data.frame()

    tags <- c("firstname", "lastname", "bib", "city", "state", "country", "age", "sex", "overall", "oversex", "overdiv")
    start <- 0
    size <- 200
    while (start < totalRecords) {
      length <- min(size, totalRecords - start) 
      print(sprintf("start, length: %d, %d", start, length))
      url <- getQuery(start, length)
      p0 <- getURL(url)
      thisData <- stripJQ(p0)
      data0 <- thisData$aaData
      data <- data0[,tags]

      data$elapsed <- data0$splits$"6210"$elapsed
      data$elapsedTime <- timestr(data$elapsed)
      data$start <- data0$start$time_ms
      data$startTime <- timestr(data0$start$time_ms)

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
  # UMI? typo? I think they mean USA
  allData[allData$country == "UMI", c("country")] <- c("USA")

  # Three records mixed up country and city assignments (I guess).
  allData[allData$country == "", c("country")] <- allData[allData$country == "", c("city")]
  allData[grepl("^KEN", allData$country), c("country")] <- c("KEN")
  allData[grepl("^ERI", allData$country), c("country")] <- c("ERI")

  # Drop entrants 5 and younger (stroller participants?)
  allData <- dplyr::filter(allData, age >= 5)
 
  # Drop records with elapsed > 3 hours, 30 minutes
  allData <- dplyr::filter(allData, elapsed < 2.75 * 3600 * 1000)

  # Drop ridiculous recrds that started before the race start time (8:30AM)
  allData <- dplyr::filter(allData, start > 30090000)

  return(allData);
}

