#!/usr/bin/env Rscript

devtools::load_all('./R')

daily_data <- readDataFiles()
daily_timestamps <- getTimestampsLabels(daily_data)

print('Normalizing dataset')
daily <- normalizeColumns(daily_data)
for (addedTimespan in 2:30) {
  print(paste('Adding timespan ', addedTimespan))
  daily <- addTimespanWindow(daily, timespan = addedTimespan)
}

save(daily, file="./playground/daily.rda")
save(daily_timestamps, file="./playground/daily_timestamps.rda")
