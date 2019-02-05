#
# data set normalization
#

getNormalizedIdColumns <- function() {
  c('symbol', 'timestamp_pos','timespan')
}

getNormalizedValueColumns <- function () {
  c(
    'prc_open','volume','prc_close','prc_close_rel',
    'prc_min', 'prc_min_rel', 'prc_max', 'prc_max_rel', 'prc_var', 'prc_var_rel'
  )
}

getNormalizedColumns <- function() {
  c('id', getNormalizedIdColumns(), 'timestamp', getNormalizedValueColumns())
}

getDataRecordId <- function (symbol, timestamp_pos, timespan) {
  paste(symbol, timestamp_pos, timespan, sep = '-')
}

getTimestampsVector <- function (gpwData) {
  sort(unique(gpwData$timestamp), decreasing = FALSE)
}

getTimestampsLabels <- function (gpwData) {
  timestamps = getTimestampsVector(gpwData)
  sequence = as.list(seq_along(timestamps))
  names(sequence) <- strftime(timestamps, tz = 'UTC')
  sequence
}

getSymbols <- function (gpwData) {
  levels(factor(gpwData$symbol))
}

addCalculatedColumns <- function (gpwData) {
  gpwData$prc_max_rel    <- gpwData$prc_max / gpwData$prc_open - 1
  gpwData$prc_min_rel    <- gpwData$prc_min / gpwData$prc_open - 1
  gpwData$prc_close_rel  <- gpwData$prc_close / gpwData$prc_open - 1
  gpwData$prc_var        <- gpwData$prc_max - gpwData$prc_min
  gpwData$prc_var_rel    <- gpwData$prc_var / gpwData$prc_open - 1
  gpwData
}

normalizeColumns <- function (gpwData, timestamps) {
  if(missing(timestamps)) {
    timestamps <- getTimestampsLabels(gpwData)
  }  

  normalized = cbind(
    gpwData,
    timestamp_pos = unlist(
      x = timestamps[strftime(gpwData$timestamp, tz = 'UTC')],
      recursive = F,
      use.names = F
    )
  )

  normalized$symbol = factor(normalized$symbol)

  normalized$id <- getDataRecordId(
    symbol = normalized$symbol,
    timestamp_pos = normalized$timestamp_pos,
    timespan = normalized$timespan
  )

  normalized <- addCalculatedColumns(normalized)

  normalized[,getNormalizedColumns()]
}

addMissingRecords <- function (gpwData) {
  dataSymbolsVector <- getSymbols(gpwData)
  timestampsVector <- unique(gpwData$timestamp_pos)
  timespanVector <- unique(gpwData$timespan)

  result <- expand.grid(
    symbol = dataSymbolsVector,
    timestamp_pos = timestampsVector,
    timespan = timespanVector
  )
  result$id <- getDataRecordId(
    symbol = result$symbol,
    timestamp_pos = result$timestamp_pos,
    timespan = result$timespan
  )
  result[,getNormalizedValueColumns()] = as.numeric(NA)
  result$timestamp = as.POSIXct(NA)
  normalized <- result[,getNormalizedColumns()]

  normalized[normalized$id %in% gpwData$id,] <- gpwData
  normalized
}

addTimespanWindow <- function (gpwData, timespan) {
  additionalTimespan <- 1
  baseTimespan <- timespan - additionalTimespan
  if(baseTimespan < 0) stop('Invalid timespan: invalid value')

  baseData <- gpwData[gpwData$timespan == baseTimespan,]
  if(nrow(baseData) == 0) stop('Invalid timespan: no base data')

  dataSymbolsVector <- getSymbols(baseData)
  timestampsPosVector <- sort(unique(baseData$timestamp_pos), decreasing = FALSE)

  minTimestampPos <- timestampsPosVector[1]
  maxTimestampPos <- timestampsPosVector[length(timestampsPosVector)]
  maxTimestampPosValid <- maxTimestampPos - baseTimespan + additionalTimespan - 1
  if(maxTimestampPosValid < minTimestampPos) stop('Nothing to do here')

  timestampsPosValidVector <- c(minTimestampPos:maxTimestampPosValid)
  baseRecords <- baseData[baseData$timestamp_pos %in% timestampsPosValidVector,]
  baseRecords$nextId <- getDataRecordId(
    symbol = baseRecords$symbol,
    timestamp_pos = (baseRecords$timestamp_pos + baseTimespan),
    timespan = additionalTimespan
  )

  result <- merge(
    baseRecords, gpwData, by.x = 'nextId', by.y = 'id',
    suffixes = c('', '.next'), sort = FALSE
  )

  result$timespan <- timespan
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

  rbind(gpwData, result[,getNormalizedColumns()])
}
