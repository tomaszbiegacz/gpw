#!/usr/bin/env Rscript

devtools::load_all('./R')

daily_data <- gpw.readDataFiles()
daily_sample <- as.gpw.relative(daily_data)
daily_sample <- gpw.addTimespanWindow(daily_sample, timespan = 2)

save(daily_sample, file="./playground/daily_sample.rda")
