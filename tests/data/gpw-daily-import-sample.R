library('gpw')

gpw_daily_data <- readDataFiles()
gpw_daily_sample_timestamps <- getTimestamps(gpw_daily_data)
gpw_daily_sample <- normalizeData(gpw_daily_data, gpw_daily_sample_timestamps)

save(gpw_daily_sample, file="./tests/data/gpw_daily_sample.rda")
save(gpw_daily_sample_timestamps, file="./tests/data/gpw_daily_sample_timestamps.rda")
