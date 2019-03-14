library(methods)
library(S4Vectors)

#
# Constant
#

PHI <- 1.618

#
# Operators
#

'%!in%' <- function(x,y)!('%in%'(x,y))

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

gpw.roulette <- setClass('gpw.roulette',
                         representation(
                           valueSlot = 'numeric'
                         ))

gpw.gene <- setClass('gpw.gene',
                      representation(
                        id = 'character',                                # UUID
                        stockData = 'gpw.relative',

                        # signature
                        stockName = 'character',
                        timespan = 'integer',                            # > 0
                        aggregator = 'character',

                        # value
                        pastRelativeTimePos = 'integer',                 # > 0
                        operator = 'character',
                        value = 'numeric',

                        # cache
                        stockRecords = "data.frame",
                        isEnabledForRecord = "function"
                      ))

gpw.geneList = setClass("gpw.geneList",
                  contains = "SimpleList",
                  prototype = prototype(elementType="gpw.gene")
                )

gpw.chromosone <- setClass('gpw.chromosone',
                           representation(
                             id = 'character',                           # UUID
                             stockData = 'gpw.relative',

                             # fitness
                             stockName = 'character',
                             futureRelativeTimePos = 'integer',          # > 0
                             isOptimistic = 'logical',

                             # activation
                             gene = 'gpw.geneList',

                             # cache
                             stockRecords = "data.frame"
                           ))

#
# Generics
#

setGeneric("stockRecords", function(x) standardGeneric("stockRecords"))

setGeneric("signature", function(x) standardGeneric("signature"))

# gpw.import

setGeneric("as.gpw.import", function(x) {
  standardGeneric("as.gpw.import")
})

# gpw.relative

setGeneric("as.gpw.relative", function(x, ...) {
  standardGeneric("as.gpw.relative")
}, signature = c('x'))

setGeneric("gpw.getValidSymbols", function(x, ...) {
  standardGeneric("gpw.getValidSymbols")
}, signature = c('x'))

setGeneric("gpw.getValidTimespans", function(x, ...) {
  standardGeneric("gpw.getValidTimespans")
}, signature = c('x'))

setGeneric("gpw.getTimestampPosRange", function(x, ...) {
  standardGeneric("gpw.getTimestampPosRange")
}, signature = c('x'))

setGeneric("gpw.getTimestampPosLength", function(x, ...) {
  standardGeneric("gpw.getTimestampPosLength")
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

# gpw.roulette

setGeneric("gpw.spin", function(x, randomNumberGenerator, ...) {
  standardGeneric("gpw.spin")
}, signature = c('x'))

# gpw.gene

setGeneric("as.gpw.gene", function(stockData, stockName, pastTelativeTimePos, aggregationTimespan, aggregator, operator, value, ...) {
  standardGeneric("as.gpw.gene")
}, signature = c('stockData'))

setGeneric("gpw.geneAggregatorAbsMedian", function(stockData, ...) {
  standardGeneric("gpw.geneAggregatorAbsMedian")
}, signature = c('stockData'))

setGeneric("gpw.randomGene", function(stockData, valueSdPerOperator, ...) {
  standardGeneric("gpw.randomGene")
}, signature = c('stockData'))

setGeneric("gpw.isEnabled", function(x, timePos, ...) {
  standardGeneric("gpw.isEnabled")
}, signature = c('x'))

setGeneric("gpw.mutate", function(x, mutationRate, ...) {
  standardGeneric("gpw.mutate")
}, signature = c('x'))

# gpw.chromosone

setGeneric("as.gpw.chromosone", function(stockData, stockName, futureRelativeTimePos, isOptimistic, genesCount, valueSdPerOperator, ...) {
  standardGeneric("as.gpw.chromosone")
}, signature = c('stockData'))

setGeneric("gpw.isTheSameSpiece", function(x, y, ...) {
  standardGeneric("gpw.isTheSameSpiece")
}, signature = c('x', 'y'))

setGeneric("gpw.getFitness", function(x, timestampPos,...) {
  standardGeneric("gpw.getFitness")
}, signature = c('x'))

setGeneric("gpw.mutate", function(x, mutationRate, ...) {
  standardGeneric("gpw.mutate")
}, signature = c('x'))

setGeneric("gpw.crossover", function(x, y, crossoverRate, ...) {
  standardGeneric("gpw.crossover")
}, signature = c('x'))
