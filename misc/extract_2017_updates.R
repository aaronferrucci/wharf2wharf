allData <- read.csv("w2w2017.csv", stringsAsFactors = FALSE)
# The previous version of w2w2017.csv should be copied into the misc subdirectory
# for this experiment.
oldData <- read.csv("misc/w2w2017.csv", stringsAsFactors = FALSE)

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

write.csv(added, "misc/added.csv")
write.csv(deleted, "misc/deleted.csv")
