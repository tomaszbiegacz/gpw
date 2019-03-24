context("population")

getTestDataBasic <- function () {
  dataFrame <- data.frame(
    symbol = c('11BIT', 'ABCD', '11BIT', 'ABCD', '11BIT', 'ABCD', '11BIT', 'ABCD'),
    timestamp = c(
      as.POSIXct('2016-01-04'),
      as.POSIXct('2016-01-04'),
      as.POSIXct('2016-01-04'),
      as.POSIXct('2016-01-04'),
      as.POSIXct('2016-01-05'),
      as.POSIXct('2016-01-05'),
      as.POSIXct('2016-01-05'),
      as.POSIXct('2016-01-05')
    ),
    timespan = c(2L, 2L, 3L, 3L, 2L, 2L, 3L, 3L),
    prc_open = as.numeric(c(1:8)),
    volume = as.numeric(c(1:8)),
    prc_close = as.numeric(c(1:8)),
    prc_min = as.numeric(c(1:8)),
    prc_max = as.numeric(c(1:8)),
    stringsAsFactors = FALSE
  )

  dataImport <- as.gpw.import(dataFrame)
  as.gpw.relative(dataImport)
}

test_that("as.gpw.population happy day", {
  result <- as.gpw.population(
    stockData = getTestDataBasic(),
    itemsCountInSpieceFamily = 1
  )

  expect_identical(class(result)[1], 'gpw.population')
  expect_identical(length(result@spiece), 4L)
})
