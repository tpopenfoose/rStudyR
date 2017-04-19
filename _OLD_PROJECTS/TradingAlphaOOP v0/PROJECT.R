rm(list = ls())

require(quantmod)
load(file = './data/EURUSD.xts_60.rdata')

source(file = './class/C_model1.R', encoding = 'UTF-8')

test <- C_model1$new(EURUSD.xts)

test.data <- test$prepared.data()

test$model.build()
