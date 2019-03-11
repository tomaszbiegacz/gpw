#!/usr/bin/env Rscript

ensureInstalled <- function (name) {
    if (!requireNamespace(name, quietly = TRUE))
        install.packages(name)
}

ensureInstalled('dplyr')
ensureInstalled('readr')
ensureInstalled('uuid')

# see https://cran.r-project.org/web/packages/BiocManager/vignettes/BiocManager.html
ensureInstalled('BiocManager')
BiocManager::install("S4Vectors")

#
# development
#
ensureInstalled('testthat')
ensureInstalled('devtools')
