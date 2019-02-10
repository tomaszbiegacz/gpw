#
# data set importing
#

getBasicIdColumns <- function() {
  c('symbol','timestamp','timespan')
}

getBasicColumns <- function() {
  c(
    getBasicIdColumns(),
    'prc_open','volume','prc_close','prc_min', 'prc_max'
  )
}

readDailyData <- function (filePath,
                       decimal_mark = '.',
                       date_format = '%Y%m%d',
                       tz = 'CET',
                       encoding = 'UTF-8') {
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

  result$timespan <- 1
  result[,getBasicColumns()]
}

readDataFiles <- function (filesPattern = '*.prn',
                           folderPath = './data/daily',
                           readFunction = readDailyData) {
  dataFilesPaths = list.files(
    path = folderPath,
    pattern = filesPattern,
    full.names = T)

  dplyr::bind_rows(lapply(dataFilesPaths, readFunction))
}
