#!/usr/bin/env Rscript

devtools::load_all('./R')

gpw_daily_data <- readDataFiles()
gpw_daily_sample_timestamps <- getTimestampsLabels(gpw_daily_data)

print('Normalizing dataset')
gpw_daily_sample <- normalizeColumns(gpw_daily_data)
for (addedTimespan in 2:30) {
  print(paste('Adding timespan ', addedTimespan))
  gpw_daily_sample <- addTimespanWindow(gpw_daily_sample, timespan = addedTimespan)
}

save(gpw_daily_sample, file="./playground/gpw_daily_sample.rda")
save(gpw_daily_sample_timestamps, file="./playground/gpw_daily_sample_timestamps.rda")
