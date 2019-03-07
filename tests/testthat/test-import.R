context("import")

getDailyStockIdColums <- function () {
  c('symbol','timestamp','timespan')
}

getDailyStockValueColums <- function () {
  c('prc_open','volume','prc_close','prc_min','prc_max')
}

getDailyStockValues <- function (prc_open, prc_max, prc_min, prc_close, volume) {
  list(prc_open=prc_open, volume=volume, prc_close=prc_close,prc_min=prc_min, prc_max=prc_max)
}

validateDailySample01 <- function (dailySample) {
  expect_equivalent(
    subset(
      dailySample,
      symbol == '11BIT' & timestamp == as.POSIXct('2016/01/04', 'CET') & timespan == 1,
      getDailyStockValueColums()),
    getDailyStockValues(prc_open=1.75, prc_max=1.83, prc_min=1.75, prc_close=1.83, volume=1109)
  )
  expect_equivalent(
    subset(
      dailySample,
      symbol == '11BIT' & timestamp == as.POSIXct('2016/01/05', 'CET') & timespan == 1,
      getDailyStockValueColums()),
    getDailyStockValues(prc_open=71, prc_max=71, prc_min=70, prc_close=70, volume=9282)
  )
  expect_equivalent(
    subset(
      dailySample,
      symbol == '11BIT' & timestamp == as.POSIXct('2016/01/07', 'CET') & timespan == 1,
      getDailyStockValueColums()),
    getDailyStockValues(prc_open=5.49, prc_max=5.69, prc_min=5.16, prc_close=5.59, volume=7611)
  )
  expect_equivalent(
    subset(
      dailySample,
      symbol == '11BIT' & timestamp == as.POSIXct('2016/01/08', 'CET') & timespan == 1,
      getDailyStockValueColums()),
    getDailyStockValues(prc_open=20.20, prc_max=20.20, prc_min=20.20, prc_close=20.20, volume=145)
  )

  expect_equivalent(
    subset(
      dailySample,
      symbol == 'ABCDATA' & timestamp == as.POSIXct('2016/01/07', 'CET') & timespan == 1,
      getDailyStockValueColums()),
    getDailyStockValues(prc_open=3.11, prc_max=3.14, prc_min=3.10, prc_close=3.14, volume=9977)
  )
}

#
# Tests
#

test_that("as.gpw.import happy day", {
  dailySample <- as.gpw.import(data.frame(
    symbol = 'ABCDATA',
    timestamp = as.POSIXct('2016/01/07', 'CET'),
    timespan = 1L,
    prc_open = 1.1,
    volume = 1.2,
    prc_close = 1.3,
    prc_min = 1.4,
    prc_max = 1.5,
    stringsAsFactors = FALSE
  ))
  expect_true(is.data.frame(dailySample))
  expect_true(inherits(dailySample, 'gpw.import'))
  expect_named(dailySample, c(getDailyStockIdColums(), getDailyStockValueColums()))
})

test_that("readDailyData imports daily-sample/01.prn", {
  dailySample <- gpw.readDailyData('./daily-sample/01.prn')
  expect_true(inherits(dailySample, 'gpw.import'))
  expect_identical(nrow(dailySample), 5L)

  validateDailySample01(dailySample)
})

test_that("readDataFiles imports daily-sample", {
  dailySample <- gpw.readDataFiles(folderPath = './daily-sample')
  expect_true(inherits(dailySample, 'gpw.import'))
  expect_identical(nrow(dailySample), 6L)

  validateDailySample01(dailySample)
  expect_equivalent(
    subset(
      dailySample,
      symbol == 'ABCDATA' & timestamp == as.POSIXct('2016/01/04', 'CET') & timespan == 1,
      getDailyStockValueColums()),
    getDailyStockValues(prc_open=4.11, prc_max=4.14, prc_min=4.10, prc_close=4.14, volume=1)
  )
})
