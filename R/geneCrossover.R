#
# gpw.geneCrossover
#

library(methods)

setValidity("gpw.geneCrossover", function (object) {
  isValid <- TRUE
  msg <- NULL

  xCommonSignature <- vapply(object@xCommon, gpw.signature, "")
  xOtherSignature <- vapply(object@xOther, gpw.signature, "")

  yCommonSignature <- vapply(object@yCommon, gpw.signature, "")
  yOtherSignature <- vapply(object@yOther, gpw.signature, "")

  if (isValid && length(xCommonSignature) == 0 && length(xOtherSignature) == 0 && length(yCommonSignature) == 0 && length(yOtherSignature) == 0) {
    isValid <- FALSE
    msg <- paste('Empty gene set')
  }

  if (isValid && is.unsorted(xCommonSignature)) {
    isValid <- FALSE
    msg <- paste('x common is unsorted')
  }

  if (isValid && is.unsorted(xOtherSignature)) {
    isValid <- FALSE
    msg <- paste('x other is unsorted')
  }

  if (isValid && is.unsorted(yCommonSignature)) {
    isValid <- FALSE
    msg <- paste('y common is unsorted')
  }

  if (isValid && is.unsorted(yOtherSignature)) {
    isValid <- FALSE
    msg <- paste('y other is unsorted')
  }

  if (isValid && length(intersect(xCommonSignature, xOtherSignature)) != 0) {
    isValid <- FALSE
    msg <- paste('x common and other share gene signatures', intersect(xCommonSignature, xOtherSignature))
  }

  if (isValid && length(intersect(yCommonSignature, yOtherSignature)) != 0) {
    isValid <- FALSE
    msg <- paste('y common and other share gene signatures', intersect(yCommonSignature, yOtherSignature))
  }

  if (isValid && !setequal(xCommonSignature, yCommonSignature)) {
    isValid <- FALSE
    msg <- paste('Invalid common signatures are not equal')
  }

  if (isValid && length(intersect(xOtherSignature, yOtherSignature)) != 0) {
    isValid <- FALSE
    msg <- paste('other sets share gene signatures', intersect(xOtherSignature, yOtherSignature))
  }

  if (isValid) TRUE else msg
})

setMethod("as.gpw.geneCrossover",
          c(x = "gpw.geneList", y = "gpw.geneList"),
          function(x, y) {
            xPos <- 1
            xLength <- length(x)
            xSignature <- vapply(x, gpw.signature, "")
            if (xLength < 1) stop('Empty x')
            if (is.unsorted(xSignature)) stop(paste('Invalid x, unsorted:', x))
            xCommonPos <- NULL
            xOtherPos <- NULL

            yPos <- 1
            yLength <- length(y)
            ySignature <- vapply(y, gpw.signature, "")
            if (yLength < 1) stop('Empty y')
            if (is.unsorted(ySignature)) stop(paste('Invalid y, unsorted:', x))
            yCommonPos <- NULL
            yOtherPos <- NULL
            while (xPos <= xLength && yPos <= yLength) {
              xPosSignature <- xSignature[[xPos]]
              xCommonPos[xPos] <- FALSE
              xOtherPos[xPos] <- FALSE

              yPosSignature <- ySignature[[yPos]]
              yCommonPos[yPos] <- FALSE
              yOtherPos[yPos] <- FALSE

              if (xPosSignature == yPosSignature) {
                xCommonPos[xPos] <- TRUE
                xPos <- xPos + 1

                yCommonPos[yPos] <- TRUE
                yPos <- yPos + 1
              }
              else {
                if (xPosSignature > yPosSignature) {
                  yOtherPos[yPos] <- TRUE
                  yPos <- yPos + 1
                }
                else {
                  xOtherPos[xPos] <- TRUE
                  xPos <- xPos + 1
                }
              }
            }

            while (xPos <= xLength) {
              xCommonPos[xPos] <- FALSE
              xOtherPos[xPos] <- TRUE
              xPos <- xPos + 1
            }

            while (yPos <= yLength) {
              yCommonPos[yPos] <- FALSE
              yOtherPos[yPos] <- TRUE
              yPos <- yPos + 1
            }

            xCommon <- x[xCommonPos]
            xOther <- x[xOtherPos]
            yCommon <- y[yCommonPos]
            yOther <- y[yOtherPos]

            gpw.geneCrossover(
              xCommon = xCommon,
              xOther = xOther,
              yCommon = yCommon,
              yOther = yOther
            )
          })

crossoverCommon <- function (x, y) {
  result <- list()
  if (length(x) > 0) {
    for(pos in c(1:length(x))) {
      result <- c(result, gpw.crossover(x[[pos]], y[[pos]]))
    }
  }
  gpw.geneList(listData = result)
}

crossoverOthers <- function (others, selectionRate, randomNumberGenerator) {
  selectedOthers <- NULL
  if(length(others) > 0) {
    for (pos in 1:length(others)) {
      selectedNumber <- randomNumberGenerator()
      selectedOthers[pos] <- selectionRate >= selectedNumber
    }
  }
  others[selectedOthers]
}

setMethod("gpw.reproduce",
          c(x = "gpw.geneCrossover"),
          function(x, otherSelectionRate, randomNumberGenerator) {
            selectionRate <- if (missing(otherSelectionRate)) 0.5 else otherSelectionRate
            numberGenerator <- if (missing(randomNumberGenerator)) (function() runif(1)) else randomNumberGenerator

            common <- crossoverCommon(x@xCommon, x@yCommon)
            xOther <- crossoverOthers(x@xOther, selectionRate, numberGenerator)
            yOther <- crossoverOthers(x@yOther, selectionRate, numberGenerator)
            c(common, xOther, yOther)
          })
