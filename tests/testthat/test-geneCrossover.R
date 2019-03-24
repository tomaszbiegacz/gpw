context("geneCrossover")

test_that("as.gpw.geneCrossover happy day", {
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
  stockData <- as.gpw.relative(dataImport)

  common_12m_1 <- as.gpw.gene(
    stockData = stockData,
    stockName = '11BIT',
    aggregationTimespan = 2L,
    aggregator = 'prc_min_rel',
    pastRelativeTimePos = 1L,
    operator = '=',
    value = 1
  )

  common_12m_2 <- as.gpw.gene(
    stockData = stockData,
    stockName = '11BIT',
    aggregationTimespan = 2L,
    aggregator = 'prc_min_rel',
    pastRelativeTimePos = 1L,
    operator = '=',
    value = 3
  )

  common_a2x_1 <- as.gpw.gene(
    stockData = stockData,
    stockName = 'ABCD',
    aggregationTimespan = 2L,
    aggregator = 'prc_max_rel',
    pastRelativeTimePos = 1L,
    operator = '>',
    value = -1
  )

  common_a2x_2 <- as.gpw.gene(
    stockData = stockData,
    stockName = 'ABCD',
    aggregationTimespan = 2L,
    aggregator = 'prc_max_rel',
    pastRelativeTimePos = 1L,
    operator = '>',
    value = -3
  )

  other_12c <- as.gpw.gene(
    stockData = stockData,
    stockName = '11BIT',
    aggregationTimespan = 2L,
    aggregator = 'prc_close_rel',
    pastRelativeTimePos = 1L,
    operator = '>',
    value = -3
  )

  other_13c <- as.gpw.gene(
    stockData = stockData,
    stockName = '11BIT',
    aggregationTimespan = 3L,
    aggregator = 'prc_close_rel',
    pastRelativeTimePos = 1L,
    operator = '>',
    value = -3
  )

  other_a2c <- as.gpw.gene(
    stockData = stockData,
    stockName = 'ABCD',
    aggregationTimespan = 2L,
    aggregator = 'prc_close_rel',
    pastRelativeTimePos = 1L,
    operator = '>',
    value = -3
  )

  other_a3c <- as.gpw.gene(
    stockData = stockData,
    stockName = 'ABCD',
    aggregationTimespan = 3L,
    aggregator = 'prc_close_rel',
    pastRelativeTimePos = 1L,
    operator = '>',
    value = -3
  )

  firstList = gpw.geneList(listData = list(other_12c, common_12m_1, other_13c, common_a2x_1))
  secondList = gpw.geneList(listData = list(common_12m_2, other_a2c, common_a2x_2, other_a3c))
  crossover <- as.gpw.geneCrossover(firstList, secondList)

  xCommon <- gpw.geneList(listData = list(common_12m_1, common_a2x_1))
  expect_equal(xCommon, crossover@xCommon)

  xOther <- gpw.geneList(listData = list(other_12c, other_13c))
  expect_equal(xOther, crossover@xOther)

  yCommon <- gpw.geneList(listData = list(common_12m_2, common_a2x_2))
  expect_equal(yCommon, crossover@yCommon)

  yOther <- gpw.geneList(listData = list(other_a2c, other_a3c))
  expect_equal(yOther, crossover@yOther)
})

test_that("as.gpw.geneCrossover happy day", {
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
  stockData <- as.gpw.relative(dataImport)

  firstList = gpw.geneList(listData = list(
    as.gpw.gene(
      stockData = stockData,
      stockName = 'ABCD',
      aggregationTimespan = 3L,
      aggregator = 'prc_max_rel',
      pastRelativeTimePos = 2L,
      operator = '<',
      value = 0
    ),
    as.gpw.gene(
      stockData = stockData,
      stockName = 'ABCD',
      aggregationTimespan = 3L,
      aggregator = 'prc_var_rel',
      pastRelativeTimePos = 2L,
      operator = '=',
      value = 0
    ),
    as.gpw.gene(
      stockData = stockData,
      stockName = 'ABCD',
      aggregationTimespan = 3L,
      aggregator = 'volume',
      pastRelativeTimePos = 2L,
      operator = '=',
      value = 0.1
    )))

  secondList = gpw.geneList(listData = list(
    as.gpw.gene(
      stockData = stockData,
      stockName = 'ABCD',
      aggregationTimespan = 3L,
      aggregator = 'prc_max_rel',
      pastRelativeTimePos = 2L,
      operator = '=',
      value = 0
    ),
    as.gpw.gene(
      stockData = stockData,
      stockName = 'ABCD',
      aggregationTimespan = 3L,
      aggregator = 'prc_min_rel',
      pastRelativeTimePos = 2L,
      operator = '<',
      value = 0
    ),
    as.gpw.gene(
      stockData = stockData,
      stockName = 'ABCD',
      aggregationTimespan = 3L,
      aggregator = 'prc_var_rel',
      pastRelativeTimePos = 2L,
      operator = '<',
      value = 0.1
    ),
    as.gpw.gene(
      stockData = stockData,
      stockName = 'ABCD',
      aggregationTimespan = 3L,
      aggregator = 'volume',
      pastRelativeTimePos = 2L,
      operator = '=',
      value = 0.2
    )))

  crossover <- as.gpw.geneCrossover(firstList, secondList)

  expect_equal(vapply(crossover@xCommon, gpw.signature, ""), c('ABCD|3|prc_max_rel', 'ABCD|3|prc_var_rel', 'ABCD|3|volume'))
  expect_equal(vapply(crossover@yCommon, gpw.signature, ""), c('ABCD|3|prc_max_rel', 'ABCD|3|prc_var_rel', 'ABCD|3|volume'))

  expect_equal(length(crossover@xOther), 0L)
  expect_equal(vapply(crossover@yOther, gpw.signature, ""), c('ABCD|3|prc_min_rel'))
})


test_that("gpw.reproduce only common", {
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
  stockData <- as.gpw.relative(dataImport)

  common_12m_1 <- as.gpw.gene(
    stockData = stockData,
    stockName = '11BIT',
    aggregationTimespan = 2L,
    aggregator = 'prc_min_rel',
    pastRelativeTimePos = 1L,
    operator = '=',
    value = 1
  )

  common_12m_2 <- as.gpw.gene(
    stockData = stockData,
    stockName = '11BIT',
    aggregationTimespan = 2L,
    aggregator = 'prc_min_rel',
    pastRelativeTimePos = 1L,
    operator = '=',
    value = 3
  )

  common_a2x_1 <- as.gpw.gene(
    stockData = stockData,
    stockName = 'ABCD',
    aggregationTimespan = 2L,
    aggregator = 'prc_max_rel',
    pastRelativeTimePos = 1L,
    operator = '>',
    value = -1
  )

  common_a2x_2 <- as.gpw.gene(
    stockData = stockData,
    stockName = 'ABCD',
    aggregationTimespan = 2L,
    aggregator = 'prc_max_rel',
    pastRelativeTimePos = 1L,
    operator = '>',
    value = -3
  )

  other_12c <- as.gpw.gene(
    stockData = stockData,
    stockName = '11BIT',
    aggregationTimespan = 2L,
    aggregator = 'prc_close_rel',
    pastRelativeTimePos = 1L,
    operator = '>',
    value = -3
  )

  other_13c <- as.gpw.gene(
    stockData = stockData,
    stockName = '11BIT',
    aggregationTimespan = 3L,
    aggregator = 'prc_close_rel',
    pastRelativeTimePos = 1L,
    operator = '>',
    value = -3
  )

  other_a2c <- as.gpw.gene(
    stockData = stockData,
    stockName = 'ABCD',
    aggregationTimespan = 2L,
    aggregator = 'prc_close_rel',
    pastRelativeTimePos = 1L,
    operator = '>',
    value = -3
  )

  other_a3c <- as.gpw.gene(
    stockData = stockData,
    stockName = 'ABCD',
    aggregationTimespan = 3L,
    aggregator = 'prc_close_rel',
    pastRelativeTimePos = 1L,
    operator = '>',
    value = -3
  )

  crossover <- gpw.geneCrossover(
    xCommon = gpw.geneList(listData = list(common_12m_1, common_a2x_1)),
    xOther = gpw.geneList(listData = list(other_12c, other_13c)),
    yCommon = gpw.geneList(listData = list(common_12m_2, common_a2x_2)),
    yOther = gpw.geneList(listData = list(other_a2c, other_a3c))
  )

  result <- gpw.reproduce(crossover, randomNumberGenerator = (function() 0.6))
  resultSignatures <- vapply(result, gpw.signature, "")

  expectedSignatures <- vapply(list(common_12m_1, common_a2x_1), gpw.signature, "")

  expect_equal(resultSignatures, expectedSignatures)
})

test_that("gpw.reproduce only common", {
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
  stockData <- as.gpw.relative(dataImport)

  common_12m_1 <- as.gpw.gene(
    stockData = stockData,
    stockName = '11BIT',
    aggregationTimespan = 2L,
    aggregator = 'prc_min_rel',
    pastRelativeTimePos = 1L,
    operator = '=',
    value = 1
  )

  common_12m_2 <- as.gpw.gene(
    stockData = stockData,
    stockName = '11BIT',
    aggregationTimespan = 2L,
    aggregator = 'prc_min_rel',
    pastRelativeTimePos = 1L,
    operator = '=',
    value = 3
  )

  common_a2x_1 <- as.gpw.gene(
    stockData = stockData,
    stockName = 'ABCD',
    aggregationTimespan = 2L,
    aggregator = 'prc_max_rel',
    pastRelativeTimePos = 1L,
    operator = '>',
    value = -1
  )

  common_a2x_2 <- as.gpw.gene(
    stockData = stockData,
    stockName = 'ABCD',
    aggregationTimespan = 2L,
    aggregator = 'prc_max_rel',
    pastRelativeTimePos = 1L,
    operator = '>',
    value = -3
  )

  other_12c <- as.gpw.gene(
    stockData = stockData,
    stockName = '11BIT',
    aggregationTimespan = 2L,
    aggregator = 'prc_close_rel',
    pastRelativeTimePos = 1L,
    operator = '>',
    value = -3
  )

  other_13c <- as.gpw.gene(
    stockData = stockData,
    stockName = '11BIT',
    aggregationTimespan = 3L,
    aggregator = 'prc_close_rel',
    pastRelativeTimePos = 1L,
    operator = '>',
    value = -3
  )

  other_a2c <- as.gpw.gene(
    stockData = stockData,
    stockName = 'ABCD',
    aggregationTimespan = 2L,
    aggregator = 'prc_close_rel',
    pastRelativeTimePos = 1L,
    operator = '>',
    value = -3
  )

  other_a3c <- as.gpw.gene(
    stockData = stockData,
    stockName = 'ABCD',
    aggregationTimespan = 3L,
    aggregator = 'prc_close_rel',
    pastRelativeTimePos = 1L,
    operator = '>',
    value = -3
  )

  crossover <- gpw.geneCrossover(
    xCommon = gpw.geneList(listData = list(common_12m_1, common_a2x_1)),
    xOther = gpw.geneList(listData = list(other_12c, other_13c)),
    yCommon = gpw.geneList(listData = list(common_12m_2, common_a2x_2)),
    yOther = gpw.geneList(listData = list(other_a2c, other_a3c))
  )

  result <- gpw.reproduce(crossover, randomNumberGenerator = (function() 0.2))
  resultSignatures <- vapply(result, gpw.signature, "")

  expectedSignatures <- vapply(list(common_12m_1, common_a2x_1, other_12c, other_13c, other_a2c, other_a3c), gpw.signature, "")

  expect_equal(resultSignatures, expectedSignatures)
})
