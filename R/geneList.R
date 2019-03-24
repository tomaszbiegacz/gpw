#
# gpw.geneList
#

library(methods)

setMethod("as.character", "gpw.geneList", function(x) as.character(vapply(x, as.character, "")))
setMethod("show", "gpw.geneList", function(object) cat(as.character(object)))
