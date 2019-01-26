context("test-gpw")

test_that("readDailyData imports daily-sample/01.prn", {
  dailySample <- gpw::readDailyData('./daily-sample/01.prn')
  expect_true(is.data.frame(dailySample))
  expect_named(dailySample, c('symbol', 'timestamp' , getDailyStockValueColums()))
  expect_identical(nrow(dailySample), 5L)

  validateDailySample01(dailySample)
})

test_that("readDataFiles imports daily-sample", {
  dailySample <- gpw::readDataFiles(folderPath = './daily-sample')
  expect_true(is.data.frame(dailySample))
  expect_named(dailySample, c('symbol', 'timestamp' , getDailyStockValueColums()))
  expect_identical(nrow(dailySample), 6L)

  validateDailySample01(dailySample)

  expect_equivalent(
    subset(
      dailySample,
      symbol == 'ABCDATA' & timestamp == as.POSIXct('2016/01/04', 'CET'),
      getDailyStockValueColums()),
    getDailyStockValues(prc_open=4.11, prc_max=4.14, prc_min=4.10, prc_close=4.14, volume=1)
  )
})

test_that("getTimestamps imports daily-sample/01.prn", {
  dailySample <- gpw::readDailyData('./daily-sample/01.prn')
  expect_equivalent(
    gpw::getDataTimestamps(dailySample),
    list(
      '2016-01-03 23:00:00' = 1,
      '2016-01-04 23:00:00' = 2,
      '2016-01-05 23:00:00' = 3,
      '2016-01-06 23:00:00' = 4
    )
  )
})

test_that("normalizeData adds timestamp_pos", {
  dailySample <- gpw::readDailyData('./daily-sample/01.prn')
  timestamps <- gpw::getDataTimestamps(dailySample)
  normalizedDailySample <- gpw::normalizeData(dailySample, timestamps)

  expect_equivalent(
    normalizedDailySample[normalizedDailySample$timestamp == as.POSIXct('2016/01/04', 'CET'), 'timestamp_pos'],
    c(1)
  )
  expect_equivalent(
    normalizedDailySample[normalizedDailySample$timestamp == as.POSIXct('2016/01/05', 'CET'), 'timestamp_pos'],
    c(2)
  )
  expect_equivalent(
    normalizedDailySample[normalizedDailySample$timestamp == as.POSIXct('2016/01/07', 'CET'), 'timestamp_pos'],
    c(3, 3)
  )
  expect_equivalent(
    normalizedDailySample[normalizedDailySample$timestamp == as.POSIXct('2016/01/08', 'CET'), 'timestamp_pos'],
    c(4)
  )

  validateDailySample01(normalizedDailySample)
})

test_that("normalizeData changes symbol to factor", {
  dailySample <- gpw::normalizeData(gpw::readDailyData('./daily-sample/01.prn'))
  expect_equivalent(
    levels(dailySample$symbol),
    c('11BIT', 'ABCDATA')
  )
})

#
# Utilities
#

getDailyStockValueColums <- function () {
  c('prc_open', 'prc_max', 'prc_min', 'prc_close', 'volume', 'timespan', 'prc_max_rel', 'prc_min_rel', 'prc_close_rel')
}

getDailyStockValues <- function (prc_open, prc_max, prc_min, prc_close, volume) {
  list(prc_open=prc_open, prc_max=prc_max, prc_min=prc_min, prc_close=prc_close, volume=volume,
       timestamp=1, prc_max_rel=(prc_max/prc_open-1), prc_min_rel=(prc_min/prc_open-1), prc_close_rel=(prc_close/prc_open-1))
}

validateDailySample01 <- function (dailySample) {
  expect_equivalent(
    subset(
      dailySample,
      symbol == '11BIT' & timestamp == as.POSIXct('2016/01/04', 'CET'),
      getDailyStockValueColums()),
    getDailyStockValues(prc_open=1.75, prc_max=1.83, prc_min=1.75, prc_close=1.83, volume=1109)
  )
  expect_equivalent(
    subset(
      dailySample,
      symbol == '11BIT' & timestamp == as.POSIXct('2016/01/05', 'CET'),
      getDailyStockValueColums()),
    getDailyStockValues(prc_open=71, prc_max=71, prc_min=70, prc_close=70, volume=9282)
  )
  expect_equivalent(
    subset(
      dailySample,
      symbol == '11BIT' & timestamp == as.POSIXct('2016/01/07', 'CET'),
      getDailyStockValueColums()),
    getDailyStockValues(prc_open=5.49, prc_max=5.69, prc_min=5.16, prc_close=5.59, volume=7611)
  )
  expect_equivalent(
    subset(
      dailySample,
      symbol == '11BIT' & timestamp == as.POSIXct('2016/01/08', 'CET'),
      getDailyStockValueColums()),
    getDailyStockValues(prc_open=20.20, prc_max=20.20, prc_min=20.20, prc_close=20.20, volume=145)
  )

  expect_equivalent(
    subset(
      dailySample,
      symbol == 'ABCDATA' & timestamp == as.POSIXct('2016/01/07', 'CET'),
      getDailyStockValueColums()),
    getDailyStockValues(prc_open=3.11, prc_max=3.14, prc_min=3.10, prc_close=3.14, volume=9977)
  )
}
