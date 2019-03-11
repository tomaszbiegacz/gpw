context("chromosone")

getTestDataBasic <- function () {
  dataFrame <- data.frame(
    symbol = c('11BIT'),
    timestamp = c(
      as.POSIXct('2016-01-04')
    ),
    timespan = 2L,
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

test_that("as.gpw.chromosone happy day", {
  result <- as.gpw.chromosone(
    stockData = getTestDataBasic(),
    stockName = '11BIT',
    fitnessTimeShift = 1L
  )

  expect_identical(class(result)[1], 'gpw.chromosone')
  expect_true(length(result) >= 1L)
})

test_that("as.gpw.chromosone mocked", {
  result <- as.gpw.chromosone(
    stockData = getTestDataBasic(),
    stockName = '11BIT',
    fitnessTimeShift = 1L,
    genesCount = 3,
    normalizeGeneFunc = function (x) x
    )

  expect_identical(length(result), 3L)

  expect_identical(as.character(result), paste(
    '11BIT over 1 timeshift, genes:',
    paste('"', as.character(object@gene[[1]]), '"'),
    paste('"', as.character(object@gene[[2]]), '"'),
    paste('"', as.character(object@gene[[3]]), '"'),
    sep = '\n'
  ))
})

test_that("normalizeGeneList reduce", {
  stockData <- getTestDataBasic()
  gene1 <- as.gpw.gene(
    stockData = stockData,
    stockName = '11BIT',
    aggregationTimespan = 2L,
    aggregator = 'prc_close_rel',
    pastRelativeTimePos = 1L,
    operator = '>',
    value = 0.1
  )

  gene2 <- as.gpw.gene(
    stockData = stockData,
    stockName = '11BIT',
    aggregationTimespan = 2L,
    aggregator = 'prc_close_rel',
    pastRelativeTimePos = 1L,
    operator = '<',
    value = 1.1
  )

  result <- normalizeGeneList(list(gene1, gene2))
  expect_identical(length(result), 1L)
  expect_identical(result[[1]], gene1)
})

test_that("normalizeGeneList preserve", {
  stockData <- getTestDataBasic()
  gene1 <- as.gpw.gene(
    stockData = stockData,
    stockName = '11BIT',
    aggregationTimespan = 2L,
    aggregator = 'prc_close_rel',
    pastRelativeTimePos = 1L,
    operator = '>',
    value = 0.1
  )

  gene2 <- as.gpw.gene(
    stockData = stockData,
    stockName = '11BIT',
    aggregationTimespan = 2L,
    aggregator = 'prc_max_rel',
    pastRelativeTimePos = 1L,
    operator = '<',
    value = 1.1
  )

  result <- normalizeGeneList(list(gene1, gene2))
  expect_identical(length(result), 2L)
  expect_identical(result[[1]], gene1)
  expect_identical(result[[2]], gene2)
})
