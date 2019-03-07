context("gene")

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

getTestDataMutation <- function () {
  dataFrame <- data.frame(
    symbol = c('11BIT', 'ABCD'),
    timestamp = c(
      as.POSIXct('2016-01-04'),
      as.POSIXct('2016-01-05')
    ),
    timespan = c(2L, 3L),
    prc_open = c(1, 2),
    volume = c(2, 4),
    prc_close = c(1.2, 1.3),
    prc_min = c(0.8, 2.1),
    prc_max = c(1.5, 5),
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
    pastRelativeTimePos = 1L,
    aggregationTimespan = 2L,
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
    pastRelativeTimePos = 1L,
    aggregationTimespan = 2L,
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
    pastRelativeTimePos = 1L,
    aggregationTimespan = 2L,
    aggregator = 'prc_min_rel',
    operator = '=',
    value = -0.2
  )

  expect_true(gpw.isEnabled(data, 2))
})

test_that("mutateName happy day", {
  expect_identical(mutateName(c('one', 'two'), 'one', newNamePosition = 2), 'two')
  expect_identical(mutateName(c('one', 'two'), 'two', newNamePosition = 2), 'one')
})

test_that("mutateInteger happy day", {
  expect_identical(mutateInteger(10, 2, valueShift = 1.1), 3L)
  expect_identical(mutateInteger(10, 2, valueShift = 1.5), 4L)
  expect_identical(mutateInteger(10, 2, valueShift = 11.1), 10L)
  expect_identical(mutateInteger(10, 2, valueShift = -2), 1L)
})

test_that("mutateGenePart stockName", {
  gene <- as.gpw.gene(
    stockData = getTestDataMutation(),
    stockName = '11BIT',
    pastRelativeTimePos = 1L,
    aggregationTimespan = 2L,
    aggregator = 'prc_min_rel',
    operator = '=',
    value = -0.2
  )

  mutated <- mutateGenePart(gene, 1)
  expect_true(gene@id != mutated@id)
  expect_identical(gene@stockData, mutated@stockData)
  expect_true(gene@stockName != mutated@stockName)
  expect_identical(gene@pastRelativeTimePos, mutated@pastRelativeTimePos)
  expect_identical(gene@timespan, mutated@timespan)
  expect_identical(gene@aggregator, mutated@aggregator)
  expect_identical(gene@operator, mutated@operator)
  expect_identical(gene@value, mutated@value)
})

test_that("mutateGenePart pastRelativeTimePos", {
  gene <- as.gpw.gene(
    stockData = getTestDataMutation(),
    stockName = '11BIT',
    pastRelativeTimePos = 1L,
    aggregationTimespan = 2L,
    aggregator = 'prc_min_rel',
    operator = '=',
    value = -0.2
  )

  mutated <- mutateGenePart(gene, 2)
  expect_true(gene@id != mutated@id)
  expect_identical(gene@stockData, mutated@stockData)
  expect_identical(gene@stockName, mutated@stockName)
  expect_true(gene@pastRelativeTimePos != mutated@pastRelativeTimePos)
  expect_identical(gene@timespan, mutated@timespan)
  expect_identical(gene@aggregator, mutated@aggregator)
  expect_identical(gene@operator, mutated@operator)
  expect_identical(gene@value, mutated@value)
})

test_that("mutateGenePart timespan", {
  gene <- as.gpw.gene(
    stockData = getTestDataMutation(),
    stockName = '11BIT',
    pastRelativeTimePos = 1L,
    aggregationTimespan = 2L,
    aggregator = 'prc_min_rel',
    operator = '=',
    value = -0.2
  )

  mutated <- mutateGenePart(gene, 3)
  expect_true(gene@id != mutated@id)
  expect_identical(gene@stockData, mutated@stockData)
  expect_identical(gene@stockName, mutated@stockName)
  expect_identical(gene@pastRelativeTimePos, mutated@pastRelativeTimePos)
  expect_true(gene@timespan != mutated@timespan)
  expect_identical(gene@aggregator, mutated@aggregator)
  expect_identical(gene@operator, mutated@operator)
  expect_identical(gene@value, mutated@value)
})

test_that("mutateGenePart aggregator", {
  gene <- as.gpw.gene(
    stockData = getTestDataMutation(),
    stockName = '11BIT',
    pastRelativeTimePos = 1L,
    aggregationTimespan = 2L,
    aggregator = 'prc_min_rel',
    operator = '=',
    value = -0.2
  )

  mutated <- mutateGenePart(gene, 4)
  expect_true(gene@id != mutated@id)
  expect_identical(gene@stockData, mutated@stockData)
  expect_identical(gene@stockName, mutated@stockName)
  expect_identical(gene@pastRelativeTimePos, mutated@pastRelativeTimePos)
  expect_identical(gene@timespan, mutated@timespan)
  expect_true(gene@aggregator != mutated@aggregator)
  expect_identical(gene@operator, mutated@operator)
  expect_identical(gene@value, mutated@value)
})

test_that("mutateGenePart operator", {
  gene <- as.gpw.gene(
    stockData = getTestDataMutation(),
    stockName = '11BIT',
    pastRelativeTimePos = 1L,
    aggregationTimespan = 2L,
    aggregator = 'prc_min_rel',
    operator = '=',
    value = -0.2
  )

  mutated <- mutateGenePart(gene, 5)
  expect_true(gene@id != mutated@id)
  expect_identical(gene@stockData, mutated@stockData)
  expect_identical(gene@stockName, mutated@stockName)
  expect_identical(gene@pastRelativeTimePos, mutated@pastRelativeTimePos)
  expect_identical(gene@timespan, mutated@timespan)
  expect_identical(gene@aggregator, mutated@aggregator)
  expect_true(gene@operator != mutated@operator)
  expect_identical(gene@value, mutated@value)
})

test_that("mutateGenePart value", {
  gene <- as.gpw.gene(
    stockData = getTestDataMutation(),
    stockName = '11BIT',
    pastRelativeTimePos = 1L,
    aggregationTimespan = 2L,
    aggregator = 'prc_min_rel',
    operator = '=',
    value = -0.2
  )

  mutated <- mutateGenePart(gene, 6)
  expect_true(gene@id != mutated@id)
  expect_identical(gene@stockData, mutated@stockData)
  expect_identical(gene@stockName, mutated@stockName)
  expect_identical(gene@pastRelativeTimePos, mutated@pastRelativeTimePos)
  expect_identical(gene@timespan, mutated@timespan)
  expect_identical(gene@aggregator, mutated@aggregator)
  expect_identical(gene@operator, mutated@operator)
  expect_true(gene@value != mutated@value)
})

test_that("gpw.mutate happy day", {
  gene <- as.gpw.gene(
    stockData = getTestDataMutation(),
    stockName = '11BIT',
    pastRelativeTimePos = 1L,
    aggregationTimespan = 2L,
    aggregator = 'prc_min_rel',
    operator = '=',
    value = -0.2
  )

  mutated <- gpw.mutate(gene, 0.2, randomNumberGenerator = function() 0.1)
  expect_true(gene@id != mutated@id)

  notMutated <- gpw.mutate(gene, 0.2, randomNumberGenerator = function() 0.3)
  expect_true(gene@id == notMutated@id)
})

