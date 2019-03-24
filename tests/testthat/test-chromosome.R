context("chromosome")

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

test_that("as.gpw.chromosome happy day", {
  result <- as.gpw.chromosome(
    stockData = getTestDataBasic(),
    stockName = '11BIT',
    futureRelativeTimePos = 2L,
    isOptimistic = TRUE
  )

  expect_identical(class(result)[1], 'gpw.chromosome')
  expect_true(length(result) >= 1L)
})

test_that("as.gpw.chromosome mocked", {
  result <- as.gpw.chromosome(
    stockData = getTestDataBasic(),
    stockName = '11BIT',
    futureRelativeTimePos = 2L,
    isOptimistic = TRUE,
    genesCount = 3,
    normalizeGeneFunc = function (x) x
  )

  expect_identical(length(result), 3L)

  expect_identical(as.character(result), paste(
    '+ 11BIT over 2 timeshift, genes:',
    paste('"', as.character(result@gene[[1]]), '"'),
    paste('"', as.character(result@gene[[2]]), '"'),
    paste('"', as.character(result@gene[[3]]), '"'),
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

  result <- normalizeGeneList(list(gene2, gene1))
  expect_identical(length(result), 2L)
  expect_identical(result[[1]], gene1)
  expect_identical(result[[2]], gene2)
})

test_that("gpw.isTheSameSpiece happy day", {
  dataFrame <- data.frame(
    symbol = c('11BIT', 'ABCD', '11BIT', 'ABCD'),
    timestamp = c(
      as.POSIXct('2016-01-04'),
      as.POSIXct('2016-01-05'),
      as.POSIXct('2016-01-05'),
      as.POSIXct('2016-01-04')
    ),
    timespan = c(1L, 2L, 2L, 1L),
    prc_open = c(1, 2, 2, 2),
    volume = c(2, 2, 2, 2),
    prc_close = c(1.2, 2, 2, 2),
    prc_min = c(0.8, 2, 2, 2),
    prc_max = c(1.5, 2, 2, 2),
    stringsAsFactors = FALSE
  )
  dataImport <- as.gpw.import(dataFrame)
  stockData <- as.gpw.relative(dataImport)

  arg <- as.gpw.chromosome(
    stockData = stockData,
    stockName = '11BIT',
    futureRelativeTimePos = 1L,
    isOptimistic = TRUE,
    genesCount = 3,
    normalizeGeneFunc = function (x) x
  )

  expect_true(gpw.isTheSameSpiece(arg, as.gpw.chromosome(
    stockData = stockData,
    stockName = '11BIT',
    futureRelativeTimePos = 1L,
    isOptimistic = FALSE
  )))

  expect_false(gpw.isTheSameSpiece(arg, as.gpw.chromosome(
    stockData = stockData,
    stockName = '11BIT',
    futureRelativeTimePos = 2L,
    isOptimistic = TRUE
  )))
  expect_false(gpw.isTheSameSpiece(arg, as.gpw.chromosome(
    stockData = stockData,
    stockName = 'ABCD',
    futureRelativeTimePos = 1L,
    isOptimistic = FALSE
  )))

})

getTestDataFitness <- function () {
  dataFrame <- data.frame(
    symbol = c('11BIT', '11BIT'),
    timestamp = c(
      as.POSIXct('2016-01-04'),
      as.POSIXct('2016-01-05')
    ),
    timespan = c(2L, 2L),
    prc_open = c(1, 1),
    volume = c(2, 3),
    prc_close = c(1.2, 1.3),
    prc_min = c(0.8, 0.9),
    prc_max = c(1.5, 1.6),
    stringsAsFactors = FALSE
  )

  dataImport <- as.gpw.import(dataFrame)
  as.gpw.relative(dataImport)
}

getTestDataFitness <- function () {
  dataFrame <- data.frame(
    symbol = c('11BIT', '11BIT'),
    timestamp = c(
      as.POSIXct('2016-01-04'),
      as.POSIXct('2016-01-05')
    ),
    timespan = c(2L, 2L),
    prc_open = c(1, 1),
    volume = c(2, 2),
    prc_close = c(1.2, 1.2),
    prc_min = c(0.8, 0.8),
    prc_max = c(1.5, 1.5),
    stringsAsFactors = FALSE
  )

  dataImport <- as.gpw.import(dataFrame)
  as.gpw.relative(dataImport)
}


test_that("gpw.getFitness enabled positive", {
  stockData <- getTestDataFitness()
  gene1 <- as.gpw.gene(
    stockData = stockData,
    stockName = '11BIT',
    pastRelativeTimePos = 1L,
    aggregationTimespan = 2L,
    aggregator = 'prc_min_rel',
    operator = '<',
    value = -0.1
  )
  chromosome <- as.gpw.chromosome(
    stockData = stockData,
    stockName = '11BIT',
    futureRelativeTimePos = 2L,
    isOptimistic = TRUE,
    normalizeGeneFunc = function (x) list(gene1)
  )

  expect_true(gpw.isEnabled(gene1, 2))
  expect_equal(gpw.getFitness(chromosome, 2), 0.2)
})

test_that("gpw.getFitness enabled negative", {
  stockData <- getTestDataFitness()
  gene1 <- as.gpw.gene(
    stockData = stockData,
    stockName = '11BIT',
    pastRelativeTimePos = 1L,
    aggregationTimespan = 2L,
    aggregator = 'prc_min_rel',
    operator = '<',
    value = -0.1
  )
  chromosome <- as.gpw.chromosome(
    stockData = stockData,
    stockName = '11BIT',
    futureRelativeTimePos = 2L,
    isOptimistic = FALSE,
    normalizeGeneFunc = function (x) list(gene1)
  )

  expect_true(gpw.isEnabled(gene1, 2))
  expect_equal(gpw.getFitness(chromosome, 2), -0.2)
})

test_that("gpw.getFitness disabled", {
  stockData <- getTestDataFitness()
  gene1 <- as.gpw.gene(
    stockData = stockData,
    stockName = '11BIT',
    pastRelativeTimePos = 1L,
    aggregationTimespan = 2L,
    aggregator = 'prc_min_rel',
    operator = '<',
    value = -0.1
  )
  gene2 <- as.gpw.gene(
    stockData = stockData,
    stockName = '11BIT',
    pastRelativeTimePos = 1L,
    aggregationTimespan = 2L,
    aggregator = 'prc_min_rel',
    operator = '>',
    value = -0.1
  )
  chromosome <- as.gpw.chromosome(
    stockData = stockData,
    stockName = '11BIT',
    futureRelativeTimePos = 2L,
    isOptimistic = FALSE,
    normalizeGeneFunc = function (x) list(gene1, gene2)
  )

  expect_true(gpw.isEnabled(gene1, 2))
  expect_false(gpw.isEnabled(gene2, 2))
  expect_equal(gpw.getFitness(chromosome, 2), 0)
})

test_that("gpw.mutate happy day", {
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
  chromosome <- as.gpw.chromosome(
    stockData = as.gpw.relative(dataImport),
    stockName = '11BIT',
    futureRelativeTimePos = 2L,
    isOptimistic = TRUE
  )

  mutated <- gpw.mutate(chromosome, 0.2)
  expect_true(inherits(mutated, 'gpw.chromosome'))
  expect_true(chromosome@id != mutated@id)
  expect_identical(chromosome@stockData, mutated@stockData)
  expect_identical(chromosome@stockName, mutated@stockName)
  expect_identical(chromosome@futureRelativeTimePos, mutated@futureRelativeTimePos)
  expect_identical(chromosome@isOptimistic, mutated@isOptimistic)
})



test_that("gpw.crossover empty", {
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
  chromosome1 <- as.gpw.chromosome(
    stockData = stockData,
    stockName = '11BIT',
    futureRelativeTimePos = 2L,
    isOptimistic = TRUE
  )
  chromosome2 <- as.gpw.chromosome(
    stockData = stockData,
    stockName = 'ABCD',
    futureRelativeTimePos = 3L,
    isOptimistic = FALSE
  )

  result <- gpw.crossover(chromosome1, chromosome2, crossoverRate = 0.3, randomNumberGenerator = (function () 0.4))
  expect_identical(length(result), 0L)
})

test_that("gpw.crossover happy day", {
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
  chromosome1 <- as.gpw.chromosome(
    stockData = stockData,
    stockName = '11BIT',
    futureRelativeTimePos = 2L,
    isOptimistic = TRUE
  )
  chromosome2 <- as.gpw.chromosome(
    stockData = stockData,
    stockName = 'ABCD',
    futureRelativeTimePos = 3L,
    isOptimistic = FALSE
  )

  result <- gpw.crossover(chromosome1, chromosome2, crossoverRate = 0.5, randomNumberGenerator = (function () 0.4))
  expect_identical(length(result), 2L)

  expect_identical(result[[1]]@stockData, stockData)
  expect_identical(result[[1]]@stockName, chromosome1@stockName)
  expect_identical(result[[1]]@futureRelativeTimePos, chromosome1@futureRelativeTimePos)
  expect_identical(result[[1]]@isOptimistic, chromosome1@isOptimistic)

  expect_identical(result[[2]]@stockData, stockData)
  expect_identical(result[[2]]@stockName, chromosome2@stockName)
  expect_identical(result[[2]]@futureRelativeTimePos, chromosome2@futureRelativeTimePos)
  expect_identical(result[[2]]@isOptimistic, chromosome2@isOptimistic)

  expect_identical(result[[1]]@gene, result[[2]]@gene)
})
