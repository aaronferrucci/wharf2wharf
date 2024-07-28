library(ggplot2)
source("w2w_utils.R")

plot_2024 <- function(filename, title) {
  data <- read.csv(filename, sep="\t")
  
  # A bunch of records are incomplete: no elapsed time. They're at the end of the place
  # data, in order of bib number. I'll assume they were no-shows.
  # remove no-shows
  data <- data[data$Chip.Elapsed.Time != "",]
  
  # add elapsed time in seconds
  elapsed <- numeric(0)
  for (i in 1:nrow(data)) {
    # just getting the numbers
    # strsplit delivers a list of list (I guess) so have to unlist
    # result is strings, so convert to ingeter
    chiptime <- as.integer(unlist(strsplit(data$Chip.Elapsed.Time[i], ":")))
    # seconds multiplier for hours, minutes, seconds
    mults <- ((length(chiptime)-1):0)
    mults <- 60^mults
    # multiple hour, minute, second by seconds per unit
    secs <- chiptime * mults
    # add them up
    the_sum <- cumsum(secs)
    # save the last
    elapsed[i] <- the_sum[length(the_sum)]
  }
  data$elapsed <- elapsed
  breakdelta <- 9
  minbreak <- floor(min(data$elapsed) / 60) - 1
  maxbreak <- ceiling((max(data$elapsed)) / (60*breakdelta)) * breakdelta
  # breaks <- seq(33, 160, 9) * 60
  breaks <- seq(minbreak, maxbreak, breakdelta) * 60
  labels <- timestr(breaks*1000)
  
  p <- ggplot(data, aes(x=elapsed)) + geom_histogram(binwidth=120) +
    ggtitle(title) + xlab("elapsed time (HH:MM:SS)") +
    geom_vline(aes(xintercept=data[grepl("Ferrucci", data$Full.Name),]$elapsed), color="red") +
    scale_x_continuous(breaks=breaks, labels=labels) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p)
}

p <- plot_2024("w2w2024_m50_59.txt", "M50-59")
print(p)
p <- plot_2024("w2w2024_m20_29.txt", "M20-29")
print(p)
