library(magrittr)
library(data.table)
library(lubridate)

price.txt.mode.to.data.table <- function(file.txt) {
  fread(file.txt) %>%
    setnames(c('SYMBOL', 'd', 't', 'OPEN', 'HIGH', 'LOW', 'CLOSE', 'VOL')) %>%
    extract(
      j = TIME := {
        (d * 10 ^6 + t) %>% format(scientific = FALSE) %>% as.character %>% ymd_hms(tz = 'GMT') %>%
          as.numeric
      }
    ) %>%
    extract(
      j = c('TIME', 'OPEN', 'HIGH', 'LOW', 'CLOSE'),
      with = FALSE
    )
}

price.dukas.csv.to.data.table <- function(file.csv) {
  fread(file.csv) %>%
    extract(
      j = TIME := paste(V1, V2) %>% ymd_hm(tz = 'GMT') %>% as.numeric
    ) %>%
    setnames(c('V3', 'V4', 'V5', 'V6', 'V7'), c('OPEN', 'HIGH', 'LOW', 'CLOSE', 'VOLUME')) %>%
    extract(
      j = c('TIME', 'OPEN', 'HIGH', 'LOW', 'CLOSE', 'VOLUME'),
      with = FALSE
    )
}


EURUSD_M30_PRICE <- price.dukas.csv.to.data.table('EURUSD_M30_2016.csv') %T>% print