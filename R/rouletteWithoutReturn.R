#
# gpw.rouletteWithoutReturn
#

library(methods)

gpw.rouletteWheelWithoutReturn <- function (component) {
  if(length(component) == 0) stop('Invalid component: empty')
  if(!all(component > 0)) stop('Invalid component: not positive')

  rouletteIds <- names(component)
  if(length(component) != length(rouletteIds)) stop('All components should have a name')

  rouletteComponents <- vapply(unname(component), function (x) x, 1)
  roulette <- gpw.rouletteWheel(rouletteComponents)

  gpw.rouletteWithoutReturn(
    roulette = roulette,
    ids = rouletteIds,
    components = rouletteComponents,
    usedPositions = 0L
  )
}

getNotUsedPositons <- function (length, usedPositions) {
  if(length < 1) stop('Invalid length')
  vapply(c(1:length), function (x) x %!in% usedPositions, FALSE)
}

rebuildRoulette <- function (x) {
  notUsedPositions <- getNotUsedPositons(length(x$ids), x$usedPositions)
  ids <- x$ids[notUsedPositions]
  components <- x$components[notUsedPositions]
  roulette <- gpw.rouletteWheel(components)

  x$ids <- ids
  x$components <- components
  x$roulette <- roulette
  x$usedPositions <- 0L
}

setMethod("gpw.spin",
          c(x = "gpw.rouletteWithoutReturn"),
          function(x, randomNumberGenerator) {
            resultPos <- gpw.spin(x$roulette, randomNumberGenerator = randomNumberGenerator)
            if (resultPos %in% x$usedPositions) {
              # didn't work out, let's try one more time
              resultPos <- gpw.spin(x$roulette, randomNumberGenerator = randomNumberGenerator)
              if (resultPos %in% x$usedPositions) {
                # I am tired, let's rebuild it
                rebuildRoulette(x)
                resultPos <- gpw.spin(x$roulette, randomNumberGenerator = randomNumberGenerator)
              }
            }

            x$usedPositions <- c(x$usedPositions, resultPos)
            x$ids[resultPos]
          })
