rm(list = ls())
library(quantmod)

load('./Data/eurusd60.rdata')

source('S1_Output.R', encoding = 'UTF-8')
source('S2_Input.R', encoding = 'UTF-8')
source('S3_Clean.R', encoding = 'UTF-8')
source('S4_Balancing.R', encoding = 'UTF-8')
source('S5_Preprocessing.R', encoding = 'UTF-8')
source('S6_Model.R', encoding = 'UTF-8')



## {
output <- Output(t.EURUSD)

input <- Input(t.EURUSD)

clean <- Clean(input, output)

balanced <- Balancing(clean)

pre <- Preprocessing(balanced)

model <- SAE.Model(pre, times = 1)


## }


## ToDo
## 1. 检验目前的ZigZag是否重绘