allData <- read.csv("w2w2017.csv", stringsAsFactors = FALSE)
oldData <- read.csv("w2w2017.csv.save", stringsAsFactors = FALSE)

# recently added:
# print bibs in the current data, not in the old data
added <- data.frame()
for (bib in allData$bib) {
  if (! bib %in% oldData$bib) {
    added <- bind_rows(added, allData[allData$bib == bib,])
  }
}

# recently deleted:
# print bibs in the old data, not in the current data
deleted <- data.frame()
for (bib in oldData$bib) {
  if (! bib %in% allData$bib) {
    deleted <- bind_rows(deleted, oldData[oldData$bib == bib,]);
  }
}
