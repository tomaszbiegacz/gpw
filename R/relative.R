#
# gpw.relative
#

library(methods)

getGpwRelativeColumnTypes <- function() {
  list(
    'id' = 'character',
    'symbol' = 'factor',
    'timestamp_pos' = 'integer',
    'timespan' = 'integer',
    'timestamp' = c('POSIXct', 'POSIXt'),
    'prc_open' = 'numeric',
    'volume' = 'numeric',
    'prc_close' = 'numeric',
    'prc_close_rel' = 'numeric',
    'prc_min' = 'numeric',
    'prc_min_rel' = 'numeric',
    'prc_max' = 'numeric',
    'prc_max_rel' = 'numeric',
    'prc_var' = 'numeric',
    'prc_var_rel' = 'numeric'
  )
}

getGpwRelativeColumnNames <- function() {
  names(getGpwRelativeColumnTypes())
}

getGpwRelativeValuesColumnNames <- function() {
  all_names <- getGpwRelativeColumnNames()
  all_names[!all_names %in% c('id', 'symbol', 'timestamp_pos', 'timespan')]
}

setValidity("gpw.relative", function (object) {
  isValid <- TRUE
  msg <- NULL

  expectedTypes <- getGpwRelativeColumnTypes()
  objectNames <- colnames(object)
  if (!identical(objectNames, names(expectedTypes))) {
    isValid <- FALSE
    msg <- paste('Invalid columns:', objectNames, 'expected', names(expectedTypes))
  }
  else {
    for(objectName in objectNames) {
      expectedType <- expectedTypes[[objectName]]
      currentType <- class(object[[objectName]])
      if (!identical(expectedType, currentType)) {
        isValid <- FALSE
        msg <- c(msg, paste('Expected type', expectedType, 'for column', objectName, 'got', currentType))
      }
    }
  }

  timestampPosRange = c(min(object$timestamp_pos), max(object$timestamp_pos))
  if (isValid && !identical(timestampPosRange, object@validTimestampsPosRange)) {
    isValid <- FALSE
    msg <- paste('Invalid slot value validTimestampsPosRange, expected', timestampPosRange, 'got', object@validTimestampsPosRange);
  }

  expectedTimestamps = object@validTimestampsPosRange[2] - object@validTimestampsPosRange[1] + 1
  if (isValid && length(object@validTimestamps) != expectedTimestamps) {
    isValid <- FALSE
    msg <- paste('Invalid number of slot values validTimestamps, expected', expectedTimestamps, 'got', length(object@validTimestamps));
  }

  expectedTimespans = unique(object$timespan)
  if (isValid && !identical(expectedTimespans, object@validTimespans)) {
    isValid <- FALSE
    msg <- paste('Invalid slot value validTimespans, expected', expectedTimespans, 'got', object@validTimespans);
  }

  if (isValid) TRUE else msg
})

getTimestampsVector <- function (gpwData) {
  sort(unique(gpwData$timestamp), decreasing = FALSE)
}

getTimestampsLabels <- function (validTimestamps) {
  sequence = as.list(seq_along(validTimestamps))
  names(sequence) <- strftime(validTimestamps)
  sequence
}

getTimestampPosRange <- function (timestamp_pos) {
  c(
    min(timestamp_pos),
    max(timestamp_pos)
  )
}

getDataRecordId <- function (symbol, timestamp_pos, timespan) {
  paste(symbol, timestamp_pos, timespan, sep = '-')
}

addCalculatedColumns <- function (gpwData) {
  gpwData$prc_max_rel    <- gpwData$prc_max / gpwData$prc_open - 1
  gpwData$prc_min_rel    <- gpwData$prc_min / gpwData$prc_open - 1
  gpwData$prc_close_rel  <- gpwData$prc_close / gpwData$prc_open - 1
  gpwData$prc_var        <- gpwData$prc_max - gpwData$prc_min
  gpwData$prc_var_rel    <- gpwData$prc_var / gpwData$prc_open - 1
  gpwData
}

setMethod("as.gpw.relative",
          c(x = "gpw.import"),
          function(x) {
            validTimestamps <- getTimestampsVector(x)
            timestampsLabels <- getTimestampsLabels(validTimestamps)
            normalized = cbind(
              x,
              timestamp_pos = unlist(
                x = timestampsLabels[strftime(x$timestamp)],
                recursive = F,
                use.names = F
              )
            )
            validTimestampsPosRange <- getTimestampPosRange(normalized$timestamp_pos)

            normalized$symbol = factor(normalized$symbol)

            normalized$id <- getDataRecordId(
              symbol = normalized$symbol,
              timestamp_pos = normalized$timestamp_pos,
              timespan = normalized$timespan
            )

            normalized <- addCalculatedColumns(normalized)

            gpw.relative(
              normalized[,getGpwRelativeColumnNames()],
              validTimestamps = validTimestamps,
              validTimestampsPosRange = validTimestampsPosRange,
              validTimespans = unique(normalized$timespan)
            )
          }
)

setMethod("gpw.getValidSymbols",
          c(x = "gpw.relative"),
          function (x) {
            levels(x$symbol)
          })

setMethod("gpw.getTimestampPosRange",
          c(x = "gpw.relative"),
          function (x) {
            x@validTimestampsPosRange
          })

setMethod("gpw.getTimestampFromPos",
          c(x = "gpw.relative"),
          function (x, pos) {
            x@validTimestamps[pos]
          })

setMethod("gpw.getValidTimespans",
          c(x = "gpw.relative"),
          function (x) {
            x@validTimespans
          })

setMethod("gpw.addMissingRecords",
          c(x = "gpw.relative"),
          function(x) {
            validSymbols <- gpw.getValidSymbols(x)
            validTimestampsPos <- c(x@validTimestampsPosRange[1]:x@validTimestampsPosRange[2])
            validTimespans <- x@validTimespans

            result <- expand.grid(
              symbol = validSymbols,
              timestamp_pos = validTimestampsPos,
              timespan = validTimespans
            )
            result$id <- getDataRecordId(
              symbol = result$symbol,
              timestamp_pos = result$timestamp_pos,
              timespan = result$timespan
            )
            result[,getGpwRelativeValuesColumnNames()] = as.numeric(NA)
            result$timestamp = as.POSIXct(NA)
            normalized <- result[,getGpwRelativeColumnNames()]

            normalized[normalized$id %in% x$id,] <- x
            gpw.relative(
              normalized,
              validTimestamps = x@validTimestamps,
              validTimestampsPosRange = x@validTimestampsPosRange,
              validTimespans = x@validTimespans
            )
          }
)

setMethod("gpw.addTimespanWindow",
          c(x = "gpw.relative"),
          function(x, timespan, additionalTimestamp) {
            validTimespan <- as.integer(timespan)

            if (missing(additionalTimestamp)) {
              validAdditionalTimestamp <- 1L
            }
            else {
              if (additionalTimestamp != 1) {
                stop('Not yet supported')
              }
              validAdditionalTimestamp <- additionalTimestamp
            }

            baseTimespan <- validTimespan - validAdditionalTimestamp
            if(baseTimespan %!in% x@validTimespans) stop('Invalid timespan: no base data')
            baseData <- x[x$timespan == baseTimespan,]

            validTimestampsPos <- sort(unique(x$timestamp_pos), decreasing = FALSE)
            minTimestampPos <- validTimestampsPos[1]
            maxTimestampPos <- validTimestampsPos[length(validTimestampsPos)]
            maxTimestampPosValid <- maxTimestampPos - baseTimespan + validAdditionalTimestamp - 1
            if(maxTimestampPosValid < minTimestampPos) stop('Nothing to do here')

            timestampsPosValidVector <- c(minTimestampPos:maxTimestampPosValid)
            baseRecords <- baseData[baseData$timestamp_pos %in% timestampsPosValidVector,]
            baseRecords$nextId <- getDataRecordId(
              symbol = baseRecords$symbol,
              timestamp_pos = (baseRecords$timestamp_pos + baseTimespan),
              timespan = validAdditionalTimestamp
            )

            result <- merge(
              baseRecords, x, by.x = 'nextId', by.y = 'id',
              suffixes = c('', '.next'), sort = FALSE
            )

            result$timespan <- validTimespan
            result$id <- getDataRecordId(
              symbol = result$symbol,
              timestamp_pos = result$timestamp_pos,
              timespan = result$timespan
            )

            # prc_open stays the same
            result$volume <- result$volume + result$volume.next
            result$prc_close <- result$prc_close.next
            result$prc_min <- pmin(result$prc_min, result$prc_min.next)
            result$prc_max <- pmax(result$prc_max, result$prc_max.next)
            result <- addCalculatedColumns(result)

            gpw.relative(
              rbind(x, result[,getGpwRelativeColumnNames()]),
              validTimestamps = x@validTimestamps,
              validTimestampsPosRange = x@validTimestampsPosRange,
              validTimespans = c(x@validTimespans, validTimespan)
            )
          }
)
