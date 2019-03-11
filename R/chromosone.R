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

  if (isValid && (object@fitnessTimeShift < 1 || object@fitnessTimeShift > gpw.getTimestampPosLength(object@stockData))) {
    isValid <- FALSE
    msg <- paste('Invalid fitnessTimeShift:', object@fitnessTimeShift)
  }

  if (isValid && length(object@gene) < 1) {
    isValid <- FALSE
    msg <- paste('invalid gene:', str(object@gene))
  }

  if (isValid) TRUE else msg
})

setMethod("as.character",
          "gpw.chromosone",
          function(x) paste(
            paste(object@stockName, 'over', object@fitnessTimeShift, 'timeshift, genes:'), 
            paste(lapply(object@gene, function(x) paste('"', as.character(x), '"')), collapse = '\n'),
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
  geneList[resultMap]
}

setMethod("as.gpw.chromosone",
          c(stockData = "gpw.relative"),
          function (stockData, stockName, fitnessTimeShift, genesCount, valueSdPerOperator, normalizeGeneFunc) {
            normalizeGene <- if(missing(normalizeGeneFunc)) normalizeGeneList else normalizeGeneFunc
            actualGenesCount <- if(missing(genesCount)) gpw.randomInteger(10) else genesCount
            medians <- if (missing(valueSdPerOperator)) gpw.geneAggregatorAbsMedian(stockData) else valueSdPerOperator
            geneList <- lapply(c(1:actualGenesCount), function (x) gpw.randomGene(stockData, valueSdPerOperator = medians))

            gpw.chromosone(
              stockData = stockData,
              stockName = stockName,
              fitnessTimeShift = fitnessTimeShift,
              gene = gpw.geneList(listData = normalizeGene(geneList))
            )
          })
