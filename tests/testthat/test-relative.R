context("relative")

getDataValueColums <- function () {
  c('symbol','timestamp_pos','timespan', 'timestamp',
    'prc_open','volume','prc_close','prc_close_rel',
    'prc_min', 'prc_min_rel', 'prc_max', 'prc_max_rel',
    'prc_var', 'prc_var_rel'
  )
}

getTestDataBasic <- function () {
  data.frame(
    symbol = c('11BIT','11BIT','11BIT','11BIT','ABCDATA'),
    timestamp = c(
      as.POSIXct('2016-01-04'),
      as.POSIXct('2016-01-05'),
      as.POSIXct('2016-01-07'),
      as.POSIXct('2016-01-08'),
      as.POSIXct('2016-01-07')
    ),
    timespan = as.integer(1),
    prc_open = c(1.75, 71.00, 5.49, 20.20, 3.11),
    volume = c(1109, 9282, 7611, 145, 9977),
    prc_close = c(1.83, 70.00, 5.59, 20.20, 3.14),
    prc_min = c(1.75, 70.00, 5.16, 20.20, 3.10),
    prc_max = c(1.83, 71.00, 5.69, 20.20, 3.14),
    stringsAsFactors = FALSE
  )
}

getTestDataBasicValidTimestamps <- function () {
  c(as.POSIXct('2016-01-04'),
    as.POSIXct('2016-01-05'),
    as.POSIXct('2016-01-07'),
    as.POSIXct('2016-01-08'))
}

getTestDataBasicValidSymbols <- function () {
  c('11BIT','ABCDATA')
}

getTestDataBasicRelative <- function () {
  src <- as.gpw.import(getTestDataBasic())
  as.gpw.relative(src)
}

getDataValues <- function (symbol, timestamp_pos, timespan, timestamp, prc_open, prc_max, prc_min, prc_close, volume) {
  list(symbol=factor(symbol), timestamp_pos=timestamp_pos, timespan=timespan, timestamp=timestamp,
       prc_open=prc_open, volume=volume, prc_close=prc_close, prc_close_rel=(prc_close/prc_open)-1,
       prc_min=prc_min, prc_min_rel=(prc_min / prc_open)-1, prc_max=prc_max, prc_max_rel=(prc_max / prc_open)-1,
       prc_var=prc_max-prc_min, prc_var_rel=((prc_max-prc_min)/prc_open)-1
  )
}

getMissingDataValues <- function (symbol, timestamp_pos, timespan) {
  list(symbol=factor(symbol), timestamp_pos=timestamp_pos, timespan=timespan, timestamp=as.POSIXct(NA),
       prc_open=as.numeric(NA), volume=as.numeric(NA), prc_close=as.numeric(NA), prc_close_rel=as.numeric(NA),
       prc_min=as.numeric(NA), prc_min_rel=as.numeric(NA), prc_max=as.numeric(NA), prc_max_rel=as.numeric(NA),
       prc_var=as.numeric(NA), prc_var_rel=as.numeric(NA)
  )
}

validateTestDataBasic <- function (dailySample) {
  expect_equivalent(
    dailySample[dailySample$id == '11BIT-1-1', getDataValueColums()],
    getDataValues(symbol='11BIT', timestamp_pos=1, timespan=1, timestamp=as.POSIXct('2016-01-04'),
                  prc_open=1.75, prc_max=1.83, prc_min=1.75, prc_close=1.83, volume=1109)
  )
  expect_equivalent(
    dailySample[dailySample$id == '11BIT-2-1', getDataValueColums()],
    getDataValues(symbol='11BIT', timestamp_pos=2, timespan=1, timestamp=as.POSIXct('2016-01-05'),
                  prc_open=71, prc_max=71, prc_min=70, prc_close=70, volume=9282)
  )
  expect_equivalent(
    dailySample[dailySample$id == '11BIT-3-1', getDataValueColums()],
    getDataValues(symbol='11BIT', timestamp_pos=3, timespan=1, timestamp=as.POSIXct('2016-01-07'),
                  prc_open=5.49, prc_max=5.69, prc_min=5.16, prc_close=5.59, volume=7611)
  )
  expect_equivalent(
    dailySample[dailySample$id == '11BIT-4-1', getDataValueColums()],
    getDataValues(symbol='11BIT', timestamp_pos=4, timespan=1, timestamp=as.POSIXct('2016-01-08'),
                  prc_open=20.20, prc_max=20.20, prc_min=20.20, prc_close=20.20, volume=145)
  )

  expect_equivalent(
    dailySample[dailySample$id == 'ABCDATA-3-1', getDataValueColums()],
    getDataValues(symbol='ABCDATA', timestamp_pos=3, timespan=1, timestamp=as.POSIXct('2016-01-07'),
                  prc_open=3.11, prc_max=3.14, prc_min=3.10, prc_close=3.14, volume=9977)
  )
}

#
# Tests
#

test_that("gpw.relative validation happy day", {
  inputDataFrame <- data.frame(
    id = 'ABCDATA-2-1',
    symbol = 'ABCDATA',
    timestamp_pos = as.integer(2),
    timespan = as.integer(1),
    timestamp = as.POSIXct('2016/01/07'),
    prc_open = 1.1,
    volume = 1.2,
    prc_close = 1.3,
    prc_close_rel = 2,
    prc_min = 1.4,
    prc_min_rel = 3,
    prc_max = 1.5,
    prc_max_rel = 3,
    prc_var = 1.6,
    prc_var_rel = 4,
    stringsAsFactors = FALSE
  )
  inputDataFrame$symbol <- factor(inputDataFrame$symbol)

  dailySample <- gpw.relative(
    inputDataFrame,
    validTimestamps = as.POSIXct('2016/01/07'),
    validSymbols = 'ABCDATA'
  )
  expect_true(is.data.frame(dailySample))
  expect_true(inherits(dailySample, 'gpw.relative'))
  expect_named(dailySample, c('id', getDataValueColums()))
  expect_identical(dailySample@validTimestamps, as.POSIXct('2016/01/07'))
  expect_identical(dailySample@validSymbols, 'ABCDATA')
})

test_that("getDataRecordId generates record id", {
  df <- data.frame(
    timestamp_pos = c(1,0),
    symbol = c('test1', 'test2'),
    timespan = c(3,4)
  )
  df$id <- gpw::getDataRecordId(timestamp_pos=df$timestamp_pos, symbol=df$symbol, timespan=df$timespan)

  expect_equivalent(df[df$symbol == 'test1', 'id'], 'test1-1-3')
  expect_equivalent(df[df$symbol == 'test2', 'id'], 'test2-0-4')
})

test_that("getTimestampsVector happy day", {
  expect_equivalent(
    gpw::getTimestampsVector(getTestDataBasic()),
    c(
      as.POSIXct('2016-01-04'),
      as.POSIXct('2016-01-05'),
      as.POSIXct('2016-01-07'),
      as.POSIXct('2016-01-08')
    )
  )
})

test_that("getTimestampsLabels happy day", {
  expect_equivalent(
    gpw::getTimestampsLabels(c(
      as.POSIXct('2016-01-04'),
      as.POSIXct('2016-01-05'),
      as.POSIXct('2016-01-07'),
      as.POSIXct('2016-01-08')
    )),
    list(
      '2016-01-04 00:00:00' = 1,
      '2016-01-05 00:00:00' = 2,
      '2016-01-06 00:00:00' = 3,
      '2016-01-07 00:00:00' = 4
    )
  )
})

test_that("getSymbols happy day", {
  expect_equivalent(
    gpw::getSymbols(getTestDataBasic()),
    c('11BIT', 'ABCDATA')
  )
})

test_that("as.gpw.relative happy day", {
  src <- as.gpw.import(getTestDataBasic())
  dailySample <- as.gpw.relative(src)
  expect_true(is.data.frame(dailySample))
  expect_true(inherits(dailySample, 'gpw.relative'))
  expect_named(dailySample, c('id', getDataValueColums()))

  expect_identical(dailySample@validTimestamps, getTestDataBasicValidTimestamps())
  expect_identical(dailySample@validSymbols, getTestDataBasicValidSymbols())
  expect_identical(nrow(dailySample), 5L)

  validateTestDataBasic(dailySample)
})

test_that("gpw.addMissingRecords happy day", {
  dailySample <- gpw.addMissingRecords(getTestDataBasicRelative())
  expect_true(is.data.frame(dailySample))
  expect_true(inherits(dailySample, 'gpw.relative'))
  expect_named(dailySample, c('id', getDataValueColums()))

  expect_identical(dailySample@validTimestamps, getTestDataBasicValidTimestamps())
  expect_identical(dailySample@validSymbols, getTestDataBasicValidSymbols())
  expect_identical(nrow(dailySample), 8L)

  validateTestDataBasic(dailySample)
  expect_equivalent(
    dailySample[dailySample$id == 'ABCDATA-1-1', getDataValueColums()],
    getMissingDataValues(symbol='ABCDATA', timestamp_pos=1, timespan=1))
  expect_equivalent(
    dailySample[dailySample$id == 'ABCDATA-2-1', getDataValueColums()],
    getMissingDataValues(symbol='ABCDATA', timestamp_pos=2, timespan=1))
  expect_equivalent(
    dailySample[dailySample$id == 'ABCDATA-4-1', getDataValueColums()],
    getMissingDataValues(symbol='ABCDATA', timestamp_pos=4, timespan=1))
})

test_that("gpw.addTimespanWindow happy day", {
  dailySample <- gpw.addTimespanWindow(getTestDataBasicRelative(), timespan = 2)
  expect_true(is.data.frame(dailySample))
  expect_true(inherits(dailySample, 'gpw.relative'))
  expect_named(dailySample, c('id', getDataValueColums()))

  expect_identical(dailySample@validTimestamps, getTestDataBasicValidTimestamps())
  expect_identical(dailySample@validSymbols, getTestDataBasicValidSymbols())
  expect_identical(nrow(dailySample), 8L)

  validateTestDataBasic(dailySample)
  expect_equivalent(
    dailySample[dailySample$id == '11BIT-1-2', getDataValueColums()],
    getDataValues(symbol='11BIT', timestamp_pos=1, timespan=2, timestamp=as.POSIXct('2016-01-04'),
                  prc_open=1.75, prc_max=71.00, prc_min=1.75, prc_close=70.00, volume=10391)
  )
  expect_equivalent(
    dailySample[dailySample$id == '11BIT-2-2', getDataValueColums()],
    getDataValues(symbol='11BIT', timestamp_pos=2, timespan=2, timestamp=as.POSIXct('2016-01-05'),
                  prc_open=71.00, prc_max=71.00, prc_min=5.16, prc_close=5.59, volume=16893)
  )
  expect_equivalent(
    dailySample[dailySample$id == '11BIT-3-2', getDataValueColums()],
    getDataValues(symbol='11BIT', timestamp_pos=3, timespan=2, timestamp=as.POSIXct('2016-01-07'),
                  prc_open=5.49, prc_max=20.20, prc_min=5.16, prc_close=20.20, volume=7756)
  )


})


