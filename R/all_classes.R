library(methods)

#
# Classes
#

gpw.import <- setClass("gpw.import",
                        contains = "data.frame")

gpw.relative <- setClass("gpw.relative",
                          contains = "data.frame",
                          representation(
                            validTimestamps = "POSIXct",
                            validTimestampsPosRange = "integer",
                            validTimespans = "integer"
                          ))

gpw.gene <- setClass('gpw.gene',
                      representation(
                        id = 'character',
                        stockData = 'gpw.relative',
                        stockName = 'character',
                        pastRelativeTimePos = 'integer',
                        timespan = 'integer',
                        aggregator = 'character',
                        operator = 'character',
                        value = 'numeric',
                        stockRecords = "data.frame",
                        isEnabledForRecord = "function"
                      ))

#
# Generics
#

setGeneric("as.gpw.import", function(x) {
  standardGeneric("as.gpw.import")
})

setGeneric("as.gpw.relative", function(x, ...) {
  standardGeneric("as.gpw.relative")
}, signature = c('x'))

setGeneric("as.gpw.gene", function(stockData, stockName, pastTelativeTimePos, timespan, aggregator, operator, value, ...) {
  standardGeneric("as.gpw.gene")
}, signature = c('stockData'))


setGeneric("gpw.getValidSymbols", function(x, ...) {
  standardGeneric("gpw.getValidSymbols")
}, signature = c('x'))

setGeneric("gpw.getValidTimespans", function(x, ...) {
  standardGeneric("gpw.getValidTimespans")
}, signature = c('x'))

setGeneric("gpw.getTimestampPosRange", function(x, ...) {
  standardGeneric("gpw.getTimestampPosRange")
}, signature = c('x'))

setGeneric("gpw.getTimestampFromPos", function(x, pos, ...) {
  standardGeneric("gpw.getTimestampFromPos")
}, signature = c('x'))

setGeneric("gpw.addMissingRecords", function(x, ...) {
  standardGeneric("gpw.addMissingRecords")
}, signature = c('x'))

setGeneric("gpw.addTimespanWindow", function(x, timespan, additionalTimestamp, ...) {
  standardGeneric("gpw.addTimespanWindow")
}, signature = c('x'))


setGeneric("gpw.isEnabled", function(x, timePos, ...) {
  standardGeneric("gpw.isEnabled")
}, signature = c('x'))