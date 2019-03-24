#
# gpw.chromosomeList
#

library(methods)

setMethod("as.character", "gpw.chromosomeList", function(x) as.character(vapply(x, as.character, "")))
setMethod("show", "gpw.chromosomeList", function(object) cat(as.character(object)))
