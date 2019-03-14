#
# gpw.chromosone
#

library(methods)
library(uuid)

setValidity("gpw.chromosone", function (object) {
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
    msg <- paste('invalid gene:', str(object@gene))
  }

  if (isValid && nrow(object@stockRecords) == 0) {
    isValid <- FALSE
    msg <- paste('stockRecords is empty')
  }

  if (isValid) TRUE else msg
})

setMethod("as.character",
          "gpw.chromosone",
          function(x) paste(
            paste(if (x@isOptimistic) "+" else "-", x@stockName, 'over', x@futureRelativeTimePos, 'timeshift, genes:'),
            paste(lapply(x@gene, function(gene) paste('"', as.character(gene), '"')), collapse = '\n'),
            sep = '\n'
          )
)

setMethod("show", "gpw.chromosone", function(object) cat(as.character(object)))

setMethod("length", "gpw.chromosone", function(x) length(x@gene))

normalizeGeneList <- function (geneList) {
  usedNames = c()
  resultMap = c()
  for (gene in geneList) {
    geneSignature <- signature(gene)
    precedense <- geneSignature %!in% usedNames
    resultMap <- c(resultMap, precedense)
    if (precedense) {
      usedNames <- c(usedNames, geneSignature)
    }
  }
  filteredResult <- geneList[resultMap]
  filteredResult[order(usedNames)]
}

setMethod("as.gpw.chromosone",
          c(stockData = "gpw.relative"),
          function (stockData, stockName, futureRelativeTimePos, isOptimistic, genesCount, valueSdPerOperator, normalizeGeneFunc) {
            normalizeGene <- if(missing(normalizeGeneFunc)) normalizeGeneList else normalizeGeneFunc
            actualGenesCount <- if(missing(genesCount)) gpw.randomInteger(10) else genesCount
            medians <- if (missing(valueSdPerOperator)) gpw.geneAggregatorAbsMedian(stockData) else valueSdPerOperator
            geneList <- lapply(c(1:actualGenesCount), function (x) gpw.randomGene(stockData, valueSdPerOperator = medians))
            stockRecords <- subset(stockData, symbol == stockName & timespan == futureRelativeTimePos)

            gpw.chromosone(
              stockData = stockData,
              stockName = stockName,
              futureRelativeTimePos = futureRelativeTimePos,
              isOptimistic = isOptimistic,
              gene = gpw.geneList(listData = normalizeGene(geneList)),
              stockRecords = stockRecords
            )
          })

setMethod("gpw.isTheSameSpiece",
          c(x = "gpw.chromosone", y = "gpw.chromosone"),
          function (x, y) {
            x@stockName == y@stockName & x@futureRelativeTimePos == y@futureRelativeTimePos
          })

setMethod("gpw.getFitness",
          c(x = "gpw.chromosone"),
          function (x, timestampPos) {
            record <- subset(x@stockRecords, timestamp_pos ==  as.integer(timestampPos))
            if (nrow(record) == 0) stop(paste('Invalid timestampPos:', timestampPos, 'valid ones:', x@stockRecords$timestamp_pos))

            isEnabled <- all(vapply(x@gene, function (x) gpw.isEnabled(x, timestampPos), TRUE))
            value <- gpw.getPriceCloseRelative(record)
            if (isEnabled) (
              if (x@isOptimistic) value else -1 * value
            ) else 0
          })
