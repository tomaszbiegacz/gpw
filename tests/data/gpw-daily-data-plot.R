load("./tests/data/gpw_daily_sample.rda")
load("./tests/data/gpw_daily_sample_timestamps.rda")

plotStockValuesAbsolute <- function(company, startPostion, timespan) {
  lastPosition <- startPostion + timespan
  rows = subset(
    gpw_daily_sample,
    symbol == company & timestamp_pos >= startPostion & timestamp_pos < lastPosition,
    c('timestamp_pos', 'prc_open', 'prc_min', 'prc_max')
  )
  open <- rows$prc_open
  min <- rows$prc_min
  max <- rows$prc_max

  plot(open, col='dark green', type = 'b', xlab='days', ylab='absolute value')
  lines(min, col='blue', type = 'l', lty  = 'dashed')
  lines(max, col='red', type = 'l', lty  = 'dashed')

  title(
    main=paste(company, ' from ', names(gpw_daily_sample_timestamps)[startPostion])
  )
}

plotStockValuesAbsolute(
  company = '11BIT',
  startPostion = 1,
  timespan = 30
)
