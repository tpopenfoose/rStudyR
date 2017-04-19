## TEST001  分类模型


rm(list = ls())
library(quantmod)

load('./Data/eurusd60.rdata')

source('TEST001FUN.R', encoding = 'UTF-8')


output <- Data.Output(t.EURUSD)

# plot(Yield(t.EURUSD, output))

input <- Data.Input(t.EURUSD)
clean <- Data.Clean(input, output)
balance <- Data.Balance(clean)




