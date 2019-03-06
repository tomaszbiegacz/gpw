context("gene")

getTestDataBasic <- function () {
  dataFrame <- data.frame(
    symbol = c('11BIT'),
    timestamp = c(
      as.POSIXct('2016-01-04')
    ),
    timespan = as.integer(2),
    prc_open = c(1),
    volume = c(2),
    prc_close = c(1.2),
    prc_min = c(0.8),
    prc_max = c(1.5),
    stringsAsFactors = FALSE
  )

  dataImport <- as.gpw.import(dataFrame)
  as.gpw.relative(dataImport)
}

#
# Tests
#

test_that("operatorEquals", {
  expect_true(operatorEquals(1000, 1001))
  expect_true(operatorEquals(-1000, -1001))

  expect_false(operatorEquals(100, 101))
  expect_false(operatorEquals(-100, -101))

  expect_true(operatorEquals(100, 101, allowedDiff = 0.1))
  expect_false(operatorEquals(100, -101, allowedDiff = 0.1))
})

test_that("operatorGreater", {
  expect_false(operatorGreater(1000, 1001))
  expect_true(operatorGreater(-1000, -1001))
})

test_that("operatorSmaller", {
  expect_true(operatorSmaller(1000, 1001))
  expect_false(operatorSmaller(-1000, -1001))
})

test_that("gpw.gene validation happy day", {
  data <- as.gpw.gene(
    stockData = getTestDataBasic(),
    stockName = '11BIT',
    pastRelativeTimePos = as.integer(1),
    aggregationTimespan = as.integer(2),
    aggregator = 'prc_close_rel',
    operator = '>',
    value = 0.1
  )

  expect_true(gpw.isEnabled(data, 2))

  # resiliency
  expect_false(gpw.isEnabled(data, 3))
  expect_false(gpw.isEnabled(data, 1))
})

test_that("gpw.gene prc_min_rel <", {
  data <- as.gpw.gene(
    stockData = getTestDataBasic(),
    stockName = '11BIT',
    pastRelativeTimePos = as.integer(1),
    aggregationTimespan = as.integer(2),
    aggregator = 'prc_min_rel',
    operator = '<',
    value = -0.1
  )

  expect_true(gpw.isEnabled(data, 2))
})

test_that("gpw.gene prc_min_rel =", {
  data <- as.gpw.gene(
    stockData = getTestDataBasic(),
    stockName = '11BIT',
    pastRelativeTimePos = as.integer(1),
    aggregationTimespan = as.integer(2),
    aggregator = 'prc_min_rel',
    operator = '=',
    value = -0.2
  )

  expect_true(gpw.isEnabled(data, 2))
})
