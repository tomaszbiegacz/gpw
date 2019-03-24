#
# gpw.population
#

library(methods)
library(uuid)

setValidity("gpw.population", function (object) {
  isValid <- TRUE
  msg <- NULL

  if (isValid && length(object@spiece) < 1) {
    isValid <- FALSE
    msg <- paste('Empty spiece')
  }

  if (isValid && !all(vapply(object@spiece, function (x) identical(x@stockData, object@stockData), FALSE))) {
    isValid <- FALSE
    msg <- paste('mixed stock data')
  }

  if (isValid) {
    names <- vapply(object@spiece, gpw.signature, '')
    if (any(duplicated(names))) {
      isValid <- FALSE
      msg <- paste('duplicates in spiece signatures:', names)
    }
  }

  if (isValid) TRUE else msg
})

setMethod("as.gpw.population",
          c(stockData = "gpw.relative"),
          function (stockData, itemsCountInSpieceFamily) {
            itemsCountInFamily <- if (missing(itemsCountInSpieceFamily)) 100 else itemsCountInSpieceFamily

            symbolSet <- gpw.getValidSymbols(stockData)
            timestampPosSet <- gpw.getValidTimespans(stockData)

            spieceList <- list()
            for (stockName in symbolSet) {
              for (futureRelativeTimePos in timestampPosSet) {
                spiece <- as.gpw.spiece(stockData, stockName, futureRelativeTimePos, itemsCountInFamily)
                spieceList <- c(spieceList, spiece)
              }
            }

            gpw.population(
              id = uuid::UUIDgenerate(),
              stockData = stockData,
              spiece = gpw.spieceList(listData = spieceList)
            )
          })
