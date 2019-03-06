#
# gpw.gene
#

library(methods)
library(uuid)

'%!in%' <- function(x,y)!('%in%'(x,y))

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

  if (isValid && object@operator %!in% names(getOperators())) {
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

