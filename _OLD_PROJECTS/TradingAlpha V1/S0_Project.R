
rm(list = ls())
library(quantmod)

load('./Data/eurusd60.rdata')
# load('./Data/000897.rdata')

source('S1_Data.R', encoding = 'UTF-8')
source('S2_Model.R', encoding = 'UTF-8')






## { TEST:
output <- Data.Output(t.EURUSD)

# plot(Yield(t.EURUSD, output))

input <- Data.Input(t.EURUSD)
clean <- Data.Clean(input, output)
balance <- Data.Balance(clean)
model.pre <- Model.Preprocessing(balance)
model.building <- Model.Building(model.pre)
model.test <- Model.Testing(model.building, clean)

summary(model.test)
# chartSeries(model.test$Equity)
# histogram(sae.predict)
# print(model.building$Models[[1]]$Accuracy)
## }




## { Note:
##    Step 1: Preparing Data
##    Step 2: Models Building, Training, Testing
##    Step 3: 
## }

## { 名词
##    market <- 市场价格
##    signal <- 信号，分为：-1做空 0空仓 1做多
## }