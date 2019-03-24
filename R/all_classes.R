library(methods)
library(S4Vectors)

#
# Constant
#

PHI <- 1.618
NEW_LINE <- '\n'

#
# Operators
#

'%!in%' <- function(x,y)!('%in%'(x,y))

#
# Classes
#

gpw.import <- setClass(
  "gpw.import",
  contains = "data.frame"
)

gpw.relative <- setClass(
  "gpw.relative",
  contains = "data.frame",
  representation(
    validTimestamps = "POSIXct",
    validTimestampsPosRange = "integer",
    validTimespans = "integer"
  )
)

gpw.roulette <- setClass(
  'gpw.roulette',
   representation(
     valueSlot = 'numeric'
   )
)

gpw.rouletteWithoutReturn <- setRefClass(
  'gpw.rouletteWithoutReturn',
  fields = list(
    roulette = 'gpw.roulette',
    ids = 'character',
    components = 'numeric',
    usedPositions = 'integer'
  )
)

gpw.gene <- setClass(
  'gpw.gene',
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
  )
)

gpw.geneList = setClass(
  "gpw.geneList",
  contains = "SimpleList",
  prototype = prototype(elementType="gpw.gene")
)

gpw.geneCrossover <- setClass(
  'gpw.geneCrossover',
  representation(
    xCommon = 'gpw.geneList',
    xOther = 'gpw.geneList',

    yCommon = 'gpw.geneList',
    yOther = 'gpw.geneList'
  )
)

gpw.chromosome <- setClass(
  'gpw.chromosome',
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
 )
)

gpw.chromosomeList = setClass(
  "gpw.chromosomeList",
  contains = "SimpleList",
  prototype = prototype(elementType="gpw.chromosome")
)

gpw.spiece <- setClass(
  'gpw.spiece',
  representation(
    id = 'character',                           # UUID
    stockData = 'gpw.relative',
    stockName = 'character',
    futureRelativeTimePos = 'integer',          # > 0
    population = 'gpw.chromosomeList'
  )
)

gpw.spieceList = setClass(
  "gpw.spieceList",
  contains = "SimpleList",
  prototype = prototype(elementType="gpw.spiece")
)

gpw.population <- setClass(
  'gpw.population',
  representation(
    id = 'character',                           # UUID
    stockData = 'gpw.relative',
    spiece = 'gpw.spieceList'
  )
)

#
# Generics
#

setGeneric("stockRecords", function(x) standardGeneric("stockRecords"))

setGeneric("gpw.signature", function(x) standardGeneric("gpw.signature"))

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

setGeneric("gpw.crossover", function(x, y, crossoverRate, ...) {
  standardGeneric("gpw.crossover")
}, signature = c('x', 'y'))

# gpw.geneCrossover

setGeneric("as.gpw.geneCrossover", function(x, y, ...) {
  standardGeneric("as.gpw.geneCrossover")
}, signature = c('x', 'y'))

setGeneric("gpw.reproduce", function(x, ...) {
  standardGeneric("gpw.reproduce")
}, signature = c('x'))

# gpw.chromosome

setGeneric("as.gpw.chromosome", function(stockData, stockName, futureRelativeTimePos, isOptimistic, genesCount, valueSdPerOperator, ...) {
  standardGeneric("as.gpw.chromosome")
}, signature = c('stockData'))

setGeneric("gpw.isTheSameSpiece", function(x, y, ...) {
  standardGeneric("gpw.isTheSameSpiece")
}, signature = c('x', 'y'))

setGeneric("gpw.getFitness", function(x, timestampPos,...) {
  standardGeneric("gpw.getFitness")
}, signature = c('x'))

# gpw.spiece

setGeneric("as.gpw.spiece", function(stockData, stockName, futureRelativeTimePos,...) {
  standardGeneric("as.gpw.spiece")
}, signature = c('stockData', 'stockName', 'futureRelativeTimePos'))

# gpw.population

setGeneric("as.gpw.population", function(stockData,...) {
  standardGeneric("as.gpw.population")
}, signature = c('stockData'))
