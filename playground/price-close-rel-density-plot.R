#!/usr/bin/env Rscript

plotPriceCloseRelativeDensity <- function(daily, company, timespanUnit) {
  rows = subset(
    daily,
    symbol == company & timespan == timespanUnit,
    c('prc_close_rel')
  )

  prc_close_rel <- density(rows$prc_close_rel)
  plot(prc_close_rel, col='dark green', xlab='relative change',
       main=paste('Price change density with Normal Curve for', company, 'agg', timespanUnit))

  meanValue <- mean(rows$prc_close_rel)
  devValue <- sd(rows$prc_close_rel)
  normalDensity <- density(rnorm(length(rows$prc_close_rel), mean = meanValue, sd = devValue))
  lines(normalDensity, col='blue', lty  = 'dashed')
}

load("./playground/daily.rda")

plotPriceCloseRelativeDensity(
  daily,
  company = 'PZU',
  timespanUnit = 2
)
