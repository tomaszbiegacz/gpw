#
# gpw.gene
#

library(methods)
library(uuid)

getAggregators <- function () {
  c(
    'prc_close_rel',
    'prc_min_rel',
    'prc_max_rel',
    'prc_var_rel',
    'volume'
  )
}

operatorEquals <- function (x, y, allowedDiff) {
  if (missing(allowedDiff)) {
    validDiff <- 0.01
  }
  else {
    validDiff <- allowedDiff
  }

  diff = abs(x - y)
  base = min(abs(x), abs(y))
  relativeDiff = diff / base
  relativeDiff < validDiff
}

operatorGreater <- function (x, y) {
  x > y
}

operatorSmaller <- function (x, y) {
  x < y
}

getOperators <- function () {
  list(
    ">" = operatorGreater,
    "=" = operatorEquals,
    "<" = operatorSmaller
  )
}

getOperatorNames <- function () {
  names(getOperators())
}

setValidity("gpw.gene", function (object) {
  isValid <- TRUE
  msg <- NULL

  if (isValid && object@stockName %!in% gpw.getValidSymbols(object@stockData)) {
    isValid <- FALSE
    msg <- paste('Invalid stock name:', object@stockName)
  }

  if (isValid && (object@pastRelativeTimePos < 1 || object@pastRelativeTimePos > gpw.getTimestampPosLength(object@stockData))) {
    isValid <- FALSE
    msg <- paste('Invalid pastRelativeTimePos:', object@pastRelativeTimePos)
  }

  if (isValid && object@timespan %!in% gpw.getValidTimespans(object@stockData)) {
    isValid <- FALSE
    msg <- paste('unknown timespan:', object@timespan)
  }

  if (isValid && object@aggregator %!in% getAggregators()) {
    isValid <- FALSE
    msg <- paste('unknown agregator:', object@aggregator)
  }

  if (isValid && object@operator %!in% getOperatorNames()) {
    isValid <- FALSE
    msg <- paste('unknown operator:', object@operator)
  }

  if (isValid) TRUE else msg
})

setMethod("stockRecords", "gpw.gene", function (x) x@stockRecords)

setMethod("as.character",
          "gpw.gene",
          function(x) paste(
            x@stockName, 'with', x@aggregator, x@operator, x@value, 
            'over (', x@pastRelativeTimePos, 'timepos and', x@timespan, 'timespan', ')'
            )
)

setMethod("show", "gpw.gene", function(object) cat(as.character(object)))

setMethod("signature", "gpw.gene", function(x) paste(x@stockName, x@timespan, x@aggregator, sep='|'))

setMethod("as.gpw.gene",
          c(stockData = "gpw.relative"),
          function (stockData, stockName, pastRelativeTimePos, aggregationTimespan, aggregator, operator, value) {
            allOperators <- getOperators()
            aggregationOperator <- allOperators[[operator]]
            isEnabledForRecordFunction <- function (record) {
              recordValue <- record[[aggregator]]
              aggregationOperator(recordValue, value)
            }
            gpw.gene(
              id = uuid::UUIDgenerate(),
              stockData = stockData,
              stockName = stockName,
              timespan = aggregationTimespan,
              aggregator = aggregator,
              pastRelativeTimePos = pastRelativeTimePos,
              operator = operator,
              value = value,
              stockRecords = subset(stockData, symbol == stockName & timespan == aggregationTimespan),
              isEnabledForRecord = isEnabledForRecordFunction
            )
          })

setMethod("gpw.geneAggregatorAbsMedian",
          c(stockData = "gpw.relative"),
          function (stockData) {
            result = list()
            for (aggName in getAggregators()) {
              result[aggName] = median(abs(stockData[[aggName]]))
            }
            result
          })

setMethod("gpw.randomGene",
          c(stockData = "gpw.relative"),
          function (stockData, valueSdPerOperator) {
            selectedAggregator <- gpw.randomItem(getAggregators())
            medians <- if (missing(valueSdPerOperator)) gpw.geneAggregatorAbsMedian(stockData) else valueSdPerOperator
            aggregatorMedian <- medians[[selectedAggregator]]
            as.gpw.gene(
              stockData = stockData,
              stockName = gpw.randomItem(gpw.getValidSymbols(stockData)),
              pastRelativeTimePos = gpw.randomInteger(gpw.getTimestampPosLength(stockData)),
              aggregationTimespan = gpw.randomItem(gpw.getValidTimespans(stockData)),
              aggregator = selectedAggregator,
              operator = gpw.randomItem(getOperatorNames()),
              value = rnorm(1, sd = aggregatorMedian)
            )
          })

setMethod("gpw.isEnabled",
          c(x = "gpw.gene"),
          function(x, timePos) {
            result = FALSE
            timestampPos <- as.integer(timePos) - x@pastRelativeTimePos
            if (timestampPos > 0) {
              record <- subset(x@stockRecords, timestamp_pos ==  timestampPos)
              if (nrow(record) > 0) {
                result = x@isEnabledForRecord(record)
              }
            }
            result
          })

mutateName <- function (validNames, currentName, newNamePosition)
{
  if (length(validNames) == 1 && validNames[[1]] != currentName) {
    validNames[[1]]
  }
  else {
    if (length(validNames) <= 1) stop(paste("Invalid validNames:", validNames))

    namePostition = if (missing(newNamePosition)) gpw.randomInteger(length(validNames)) else newNamePosition
    newName <- validNames[[namePostition]]
    if (newName == currentName) {
      # this should finish at some point...
      mutateName(validNames[validNames %!in% currentName], currentName)
    }
    else
      newName
  }
}

mutateNumericPositive <- function (currentValue, valueShift)
{
  shift <- if(missing(valueShift)) rnorm(1, sd = max(1, currentValue)) else valueShift
  max(0, currentValue + shift)
}

mutateInteger <- function (maxValue, currentValue, valueShift)
{
  shift <- if(missing(valueShift)) max(1, rnorm(1, sd = max(3, currentValue))) else valueShift
  as.integer(min(maxValue, max(1, as.integer(round(currentValue + shift)))))
}

GENE_STOCK_NAME = 1
GENE_TIMESTAMP = 2
GENE_TIMESPAN = 3
GENE_AGGREGATOR = 4
GENE_OPERATOR = 5
GENE_VALUE = 6

spinGenePart <- function() {
  # distibution of probabilities at roulette wheel
  gpw.spin(c(1:6))
}

mutateGenePart <- function (x, partToMutate) {
  stockName <- if (partToMutate == GENE_STOCK_NAME)
    mutateName(gpw.getValidSymbols(x@stockData), x@stockName) else x@stockName
  pastRelativeTimePos <- if (partToMutate == GENE_TIMESTAMP)
    mutateInteger(gpw.getTimestampPosRange(x@stockData)[2], x@pastRelativeTimePos) else x@pastRelativeTimePos
  timespan <- if (partToMutate == GENE_TIMESPAN)
    mutateInteger(max(gpw.getValidTimespans(x@stockData)), x@timespan) else x@timespan
  aggregator <- if (partToMutate == GENE_AGGREGATOR)
    mutateName(getAggregators(), x@aggregator) else x@aggregator
  operator <- if (partToMutate == GENE_OPERATOR)
    mutateName(getOperatorNames(), x@operator) else x@operator
  value <- if (partToMutate == GENE_VALUE)
    mutateNumericPositive(x@value) else x@value

  as.gpw.gene(
    stockData = x@stockData,
    stockName = stockName,
    pastRelativeTimePos = pastRelativeTimePos,
    aggregationTimespan = timespan,
    aggregator = aggregator,
    operator = operator,
    value = value
  )
}

setMethod("gpw.mutate",
          c(x = "gpw.gene"),
          function(x, mutationRate, randomNumberGenerator) {
            if (missing(randomNumberGenerator))
              selectedNumber <- runif(1)
            else
              selectedNumber <- randomNumberGenerator()

            if (mutationRate >= selectedNumber)
              mutateGenePart(x, spinGenePart())
            else
              x
          })

