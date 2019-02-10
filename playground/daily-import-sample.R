#!/usr/bin/env Rscript

devtools::load_all('./R')

daily_data <- readDataFiles()
daily_sample <- normalizeColumns(daily_data)
daily_sample <- addTimespanWindow(daily_sample, timespan = 2)

save(daily_sample, file="./playground/daily_sample.rda")
