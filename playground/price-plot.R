#!/usr/bin/env Rscript

plotStockValuesAbsolute <- function(daily, dailyTimestamps,
                                    company, startPostion, timespan, timespanUnit) {
  lastPosition <- startPostion + timespan
  rows = subset(
    daily,
    symbol == company & timestamp_pos >= startPostion & timestamp_pos < lastPosition & timespan == timespanUnit,
    c('timestamp_pos', 'prc_open', 'prc_min', 'prc_max')
  )
  open <- rows$prc_open
  min <- rows$prc_min
  max <- rows$prc_max

  plot(open, col='dark green', type = 'b', xlab='days', ylab='absolute value')
  lines(min, col='blue', type = 'l', lty  = 'dashed')
  lines(max, col='red', type = 'l', lty  = 'dashed')

  title(
    main=paste(company, 'agg', timespanUnit, 'days from', strftime(dailyTimestamps[startPostion]))
  )
}

load("./playground/daily.rda")

plotStockValuesAbsolute(
  daily, daily@validTimestamps,
  company = '11BIT',
  startPostion = 1,
  timespan = 30,
  timespanUnit = 5
)
