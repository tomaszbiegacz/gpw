#!/usr/bin/env Rscript

devtools::load_all('./R')

daily_data <- gpw.readDataFiles()

print('Normalizing dataset')
daily <- as.gpw.relative(daily_data)
for (addedTimespan in 2:30) {
  print(paste('Adding timespan ', addedTimespan))
  daily <- gpw.addTimespanWindow(daily, timespan = addedTimespan)
}

save(daily, file="./playground/daily.rda")
