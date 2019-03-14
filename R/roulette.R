library(methods)

gpw.randomInteger <- function (maxValue, randomNumberGenerator)
{
  if (maxValue < 1) stop('Invalid maxValue')
  if (missing(randomNumberGenerator))
    selectedNumber <- runif(1, min=1, max=maxValue)
  else
    selectedNumber <- randomNumberGenerator(maxValue)

  as.integer(ceiling(selectedNumber))
}

gpw.randomItem <- function(items)
{
  items[gpw.randomInteger(length(items))]
}

#
# gpw.roulette
#

setValidity("gpw.roulette", function (object) {
  isValid <- TRUE
  msg <- NULL

  if (isValid && length(object@valueSlot) == 0) {
    isValid <- FALSE
    msg <- 'Invalid value slot: empty'
  }

  if (isValid) TRUE else msg
})

gpw.rouletteWheel <- function (component) {
  if(length(component) == 0) stop('Invalid component: empty')
  if(!(min(component) > 0)) stop('Invalid component: not positive')
  componentsSum <- sum(component)
  getProbability <- function (x) x/componentsSum
  cumulatedProbabilities <- vapply(cumsum(component), getProbability, 1)
  gpw.roulette(
    valueSlot = cumulatedProbabilities
  )
}

setMethod("gpw.spin",
          c(x = "gpw.roulette"),
          function(x, randomNumberGenerator) {
            if (missing(randomNumberGenerator))
              selectedNumber <- runif(1)
            else
              selectedNumber <- randomNumberGenerator()

            result <- max(1, min(length(x@valueSlot), which( order(c(selectedNumber,x@valueSlot))==1 )))
            as.integer(result)
          })

setMethod("gpw.spin",
          c(x = "numeric"),
          function(x) {
            roulette <- gpw.rouletteWheel(x)
            gpw.spin(roulette)
          })
