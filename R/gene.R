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

  timePosRange <- gpw.getTimestampPosRange(object@stockData)
  timePosLength = timePosRange[2] - timePosRange[1] + 1
  if (isValid && (object@pastRelativeTimePos < 1 || object@pastRelativeTimePos > timePosLength)) {
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
              pastRelativeTimePos = pastRelativeTimePos,
              timespan = aggregationTimespan,
              aggregator = aggregator,
              operator = operator,
              value = value,
              stockRecords = subset(stockData, symbol == stockName && timespan == aggregationTimespan),
              isEnabledForRecord = isEnabledForRecordFunction
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

mutateNumeric <- function (maxValue, currentValue, valueShift)
{
  minValue <- -1 * maxValue
  shift <- if(missing(valueShift)) rnorm(1, sd = maxValue / PHI) else valueShift
  min(maxValue, max(minValue, currentValue + shift))
}

mutateInteger <- function (maxValue, currentValue, valueShift)
{
  shift <- if(missing(valueShift)) max(1, rnorm(1, sd = maxValue / PHI)) else valueShift
  as.integer(min(maxValue, max(1, as.integer(round(currentValue + shift)))))
}

mutateGenePart <- function (x, partToMutate) {
  stockName <- if (partToMutate == 1)
    mutateName(gpw.getValidSymbols(x@stockData), x@stockName) else x@stockName
  pastRelativeTimePos <- if (partToMutate == 2)
    mutateInteger(gpw.getTimestampPosRange(x@stockData)[2], x@pastRelativeTimePos) else x@pastRelativeTimePos
  timespan <- if (partToMutate == 3)
    mutateInteger(max(gpw.getValidTimespans(x@stockData)), x@timespan) else x@timespan
  aggregator <- if (partToMutate == 4)
    mutateName(getAggregators(), x@aggregator) else x@aggregator
  operator <- if (partToMutate == 5)
    mutateName(getOperatorNames(), x@operator) else x@operator
  value <- if (partToMutate == 6)
    mutateNumeric(100, x@value) else x@value

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

            if (mutationRate >= selectedNumber) {
              genePartMutationWheel <- gpw.rouletteWheel(c(1:6))
              partToMutate <- gpw.spin(genePartMutationWheel)
              mutateGenePart(x, partToMutate)
            }
            else {
              x
            }
          })

