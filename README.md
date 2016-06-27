# wharf2wharf
Analysis of the Santa Cruz Wharf to Wharf race. See here for the rendered document: http://rpubs.com/aaronferrucci/99569

To prepare data for the WharfToWharfR package, follow this sequence:
- source("w2w_utils.R")
- data <- getData(year)
- data <- clean(year, data)
- data <- anonymize(data)
- write.csv(data, <raw data dir>)
