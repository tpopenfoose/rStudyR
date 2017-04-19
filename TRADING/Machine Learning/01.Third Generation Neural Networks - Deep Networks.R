#---------------------------------------------------------------#
# Title:  Third Generation Neural Networks: Deep Networks       #
# Source: https://www.mql5.com/zh/articles/1103                 #
# Date:   2014-11-27                                            #
# Autor:  Vladimir Perervenko                                   #
# Packages: c()                                                 #
#---------------------------------------------------------------#

required.packages <- c('magrittr', 'data.table', 'TTR')
bars <- 6000

suppressPackageStartupMessages({
  sapply(required.packages, require, quietly = FALSE, character.only = TRUE)
})

PRICE <- load('eurusd_m30.rdata') %>% get %>% tail(bars)

add.price.factor <- function(price.data.table) {
  price.data.table[, c('TIME', 'MEDIAN', 'DIFF') := list(NULL, (HIGH + LOW) * 0.5, CLOSE - OPEN)]
}

PRICE.with.median.diff <- add.price.factor(PRICE)

ADX(PRICE.with.median.diff)
