#
# gpw.chromosome
#

library(methods)
library(uuid)

setValidity("gpw.chromosome", function (object) {
  isValid <- TRUE
  msg <- NULL

  if (isValid && object@stockName %!in% gpw.getValidSymbols(object@stockData)) {
    isValid <- FALSE
    msg <- paste('Invalid stock name:', object@stockName)
  }

  if (isValid && object@futureRelativeTimePos %!in% gpw.getValidTimespans(object@stockData)) {
    isValid <- FALSE
    msg <- paste('Invalid futureRelativeTimePos:', object@futureRelativeTimePos)
  }

  if (isValid && length(object@gene) < 1) {
    isValid <- FALSE
    msg <- paste('Empty gene')
  }

  if (isValid && !all(vapply(object@gene, function (x) identical(x@stockData, object@stockData), FALSE))) {
    isValid <- FALSE
    msg <- paste('mixed stock data')
  }

  if (isValid && nrow(object@stockRecords) == 0) {
    isValid <- FALSE
    msg <- paste('stockRecords is empty')
  }

  if (isValid) TRUE else msg
})

setMethod("as.character",
          "gpw.chromosome",
          function(x) paste(
            paste(if (x@isOptimistic) "+" else "-", x@stockName, 'over', x@futureRelativeTimePos, 'timeshift, genes:'),
            paste(lapply(x@gene, function(gene) paste('"', as.character(gene), '"')), collapse = '\n'),
            sep = '\n'
          )
)

setMethod("show", "gpw.chromosome", function(object) cat(as.character(object)))

setMethod("length", "gpw.chromosome", function(x) length(x@gene))

normalizeGeneList <- function (geneList) {
  usedNames = NULL
  resultMap = NULL
  for (gene in geneList) {
    geneSignature <- gpw.signature(gene)
    precedense <- geneSignature %!in% usedNames
    resultMap <- c(resultMap, precedense)
    if (precedense) {
      usedNames <- c(usedNames, geneSignature)
    }
  }
  filteredResult <- geneList[resultMap]
  filteredResult[order(usedNames)]
}

setMethod("as.gpw.chromosome",
          c(stockData = "gpw.relative"),
          function (stockData, stockName, futureRelativeTimePos, isOptimistic, genesCount, valueSdPerOperator, normalizeGeneFunc) {
            normalizeGene <- if(missing(normalizeGeneFunc)) normalizeGeneList else normalizeGeneFunc
            actualGenesCount <- if(missing(genesCount)) gpw.randomInteger(10) else genesCount
            medians <- if (missing(valueSdPerOperator)) gpw.geneAggregatorAbsMedian(stockData) else valueSdPerOperator
            geneList <- lapply(c(1:actualGenesCount), function (x) gpw.randomGene(stockData, valueSdPerOperator = medians))
            stockRecords <- subset(stockData, symbol == stockName & timespan == futureRelativeTimePos)

            gpw.chromosome(
              id = uuid::UUIDgenerate(),
              stockData = stockData,
              stockName = stockName,
              futureRelativeTimePos = futureRelativeTimePos,
              isOptimistic = isOptimistic,
              gene = gpw.geneList(listData = normalizeGene(geneList)),
              stockRecords = stockRecords
            )
          })

setMethod("gpw.isTheSameSpiece",
          c(x = "gpw.chromosome", y = "gpw.chromosome"),
          function (x, y) {
            x@stockName == y@stockName && x@futureRelativeTimePos == y@futureRelativeTimePos
          })

setMethod("gpw.getFitness",
          c(x = "gpw.chromosome"),
          function (x, timestampPos) {
            record <- subset(x@stockRecords, timestamp_pos ==  as.integer(timestampPos))
            if (nrow(record) == 0) stop(paste('Invalid timestampPos:', timestampPos, 'valid ones:', x@stockRecords$timestamp_pos))

            isEnabled <- all(vapply(x@gene, function (x) gpw.isEnabled(x, timestampPos), TRUE))
            value <- gpw.getPriceCloseRelative(record)
            if (isEnabled) (
              if (x@isOptimistic) value else -1 * value
            ) else 0
          })

setMethod("gpw.mutate",
          c(x = "gpw.chromosome"),
          function(x, mutationRate) {
            geneList <- lapply(x@gene, function (g) gpw.mutate(g, mutationRate))
            gpw.chromosome(
              id = uuid::UUIDgenerate(),
              stockData = x@stockData,
              stockName = x@stockName,
              futureRelativeTimePos = x@futureRelativeTimePos,
              isOptimistic = x@isOptimistic,
              gene = gpw.geneList(listData = normalizeGeneList(geneList)),
              stockRecords = x@stockRecords
            )
          })

chromosomalCrossover <- function(x, y) {
  geneCrossover <- as.gpw.geneCrossover(x@gene, y@gene)
  geneReproduction <- gpw.reproduce(geneCrossover)
  geneList <- gpw.geneList(listData = normalizeGeneList(geneReproduction@listData))

  xChromosome <- gpw.chromosome(
    id = uuid::UUIDgenerate(),
    stockData = x@stockData,
    stockName = x@stockName,
    futureRelativeTimePos = x@futureRelativeTimePos,
    isOptimistic = x@isOptimistic,
    gene = geneList,
    stockRecords = x@stockRecords
  )

  yChromosome <- gpw.chromosome(
    id = uuid::UUIDgenerate(),
    stockData = y@stockData,
    stockName = y@stockName,
    futureRelativeTimePos = y@futureRelativeTimePos,
    isOptimistic = y@isOptimistic,
    gene = geneList,
    stockRecords = y@stockRecords
  )

  gpw.chromosomeList(listData = list(xChromosome, yChromosome))
}

setMethod("gpw.crossover",
          c(x = "gpw.chromosome", y = "gpw.chromosome"),
          function(x, y, crossoverRate, randomNumberGenerator) {
            if (missing(randomNumberGenerator))
              selectedNumber <- runif(1)
            else
              selectedNumber <- randomNumberGenerator()

            if (crossoverRate >= selectedNumber)
              chromosomalCrossover(x, y)
            else
              NULL
          })
