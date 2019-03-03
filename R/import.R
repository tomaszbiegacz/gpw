#
# gpw.import
#

library(methods)

getGpwImportColumnTypes <- function() {
  list(
    'symbol' = 'character',
    'timestamp' = c('POSIXct', 'POSIXt'),
    'timespan' = 'integer',
    'prc_open' = 'numeric',
    'volume' = 'numeric',
    'prc_close' = 'numeric',
    'prc_min' = 'numeric',
    'prc_max' = 'numeric'
  )
}

getGpwImportColumnNames <- function() {
  names(getGpwImportColumnTypes())
}

gpw.import <- setClass("gpw.import",
                        contains = "data.frame")

setValidity("gpw.import", function (object) {
  expectedTypes <- getGpwImportColumnTypes()

  isValid <- TRUE
  msg <- NULL
  objectNames <- colnames(object)
  if (!identical(objectNames, names(expectedTypes))) {
    isValid <- FALSE
    msg <- paste('Invalid columns:', objectNames, 'expected', names(expectedTypes))
  }
  else {
    for(objectName in objectNames) {
      expectedType <- expectedTypes[[objectName]]
      currentType <- class(object[[objectName]])
      if (!identical(expectedType, currentType)) {
        isValid <- FALSE
        msg <- c(msg, paste('Expected type', expectedType, 'for column', objectName, 'got', currentType))
      }
    }
  }

  if (isValid) TRUE else msg
})

setGeneric("as.gpw.import", function(x) {
  standardGeneric("as.gpw.import")
})

setMethod("as.gpw.import",
          c(x = "data.frame"),
          function(x) {
            gpw.import(x)
          }
)


gpw.readDailyData <- function (filePath,
                               decimal_mark = '.',
                               date_format = '%Y%m%d',
                               tz = 'CET',
                               encoding = 'UTF-8',
                               returnDataFrame = FALSE) {
  # Manuals
  # https://github.com/tidyverse/readr/blob/master/R/read_delim.R
  # https://github.com/tidyverse/readr/blob/master/R/col_types.R
  result <- as.data.frame(readr::read_csv(
    filePath,
    trim_ws = T,
    locale = readr::locale(
      decimal_mark = decimal_mark,
      date_format = date_format,
      tz = tz,
      encoding = encoding
    ),
    col_names = c('symbol', 'timestamp' , 'prc_open', 'prc_max', 'prc_min', 'prc_close', 'volume'),
    col_types = readr::cols(
      symbol = readr::col_character(),
      timestamp = readr::col_datetime(),
      prc_open = readr::col_number(),
      prc_max = readr::col_number(),
      prc_min = readr::col_number(),
      prc_close = readr::col_number(),
      volume = readr::col_number()
    )
  ))

  result$timespan <- as.integer(1)
  if (returnDataFrame) result[,getGpwImportColumnNames()] else gpw.import(result[,getGpwImportColumnNames()])
}

gpw.readDataFiles <- function (filesPattern = '*.prn',
                               folderPath = './data/daily',
                               readFunction = function(filePath) gpw.readDailyData(filePath, returnDataFrame=TRUE)) {
  dataFilesPaths = list.files(
    path = folderPath,
    pattern = filesPattern,
    full.names = T)

  gpw.import(dplyr::bind_rows(lapply(dataFilesPaths, readFunction)))
}
