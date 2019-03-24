context("spiece")

getTestDataBasic <- function () {
  dataFrame <- data.frame(
    symbol = c('11BIT', '11BIT', '11BIT', '11BIT'),
    timestamp = c(
      as.POSIXct('2016-01-04'),
      as.POSIXct('2016-01-04'),
      as.POSIXct('2016-01-05'),
      as.POSIXct('2016-01-05')
    ),
    timespan = c(2L, 3L, 2L, 3L),
    prc_open = as.numeric(c(1:4)),
    volume = as.numeric(c(1:4)),
    prc_close = as.numeric(c(1:4)),
    prc_min = as.numeric(c(1:4)),
    prc_max = as.numeric(c(1:4)),
    stringsAsFactors = FALSE
  )

  dataImport <- as.gpw.import(dataFrame)
  as.gpw.relative(dataImport)
}

test_that("as.gpw.spiece happy day", {
  result <- as.gpw.spiece(
    stockData = getTestDataBasic(),
    stockName = "11BIT",
    futureRelativeTimePos = 2L,
    itemsCountInFamily = 3
  )

  expect_identical(class(result)[1], 'gpw.spiece')
  expect_identical(length(result), 6L)
  expect_identical(gpw.signature(result), "11BIT|2")
})
