#
# gpw.spiece
#

library(methods)
library(uuid)

setMethod("gpw.isTheSameSpiece",
          c(x = "gpw.spiece", y = "gpw.chromosome"),
          function (x, y) {
            x@stockName == y@stockName && x@futureRelativeTimePos == y@futureRelativeTimePos
          })

setValidity("gpw.spiece", function (object) {
  isValid <- TRUE
  msg <- NULL

  if (isValid && length(object@population) < 1) {
    isValid <- FALSE
    msg <- paste('Empty population')
  }

  if (isValid && !all(vapply(object@population, function (x) gpw.isTheSameSpiece(object, x), FALSE))) {
    isValid <- FALSE
    msg <- paste('mixed chromosome of not the same spiece')
  }

  if (isValid) TRUE else msg
})

setMethod("as.character",
          "gpw.spiece",
          function(x) paste(x@stockName, 'over', x@futureRelativeTimePos, 'timeshift, ', length(x@population, ' items'))
)

setMethod("show", "gpw.spiece", function(object) cat(as.character(object)))

setMethod("length", "gpw.spiece", function(x) length(x@population))

setMethod("gpw.signature", "gpw.spiece", function(x) paste(x@stockName, x@futureRelativeTimePos, sep='|'))

setMethod("as.gpw.spiece",
          c(stockData = "gpw.relative", stockName = "character", futureRelativeTimePos = "integer"),
          function (stockData, stockName, futureRelativeTimePos, itemsCountInFamily) {
            populationList <- list()
            for (isOptimistic in c(FALSE, TRUE)) {
              family <- lapply(
                c(1:itemsCountInFamily),
                function(x) as.gpw.chromosome(stockData, stockName, futureRelativeTimePos, isOptimistic)
              )
              populationList <- c(populationList, family)
            }

            gpw.spiece(
              id = uuid::UUIDgenerate(),
              stockData = stockData,
              stockName = stockName,
              futureRelativeTimePos = futureRelativeTimePos,
              population = gpw.chromosomeList(listData = populationList)
            )
          })
