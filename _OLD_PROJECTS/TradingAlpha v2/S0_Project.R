
rm(list = ls())

lib.choice <- 1

if(lib.choice == 1) {
  library(quantmod)
  load('./Data/EURUSD.xts_60.rdata')
} else if(lib.choice == 2) {
  library(fPortfolio)
  load('./Data/EURUSD.ts_60.rdata')
}



source('SS_Useful.R', encoding = 'UTF-8')
source('S1_Data.R', encoding = 'UTF-8')

op <- Data.Output(EURUSD.xts)
ip <- Data.Input(EURUSD.xts)
cl <- Data.Clean(ip, op)
ba <- Data.Balance(cl)
pp <- Data.Preparing(ba)

model.test = c('recursive partitioning tree')
model <- 1

if(model == 1) (
  source('S2_Model01.R', encoding = 'UTF-8')
)


model <- Model.Build(pp)
model.estimate <- Model.Estimate(model, pp$TS)
# 
# printcp(model)
# plotcp(model)
# summary(model)
# plot(model, margin = 0.1, uniform = T, branch = 0.6)
# text(model, all = T, use.n = T)

# ts <- pp$TS
# # ts <- subset(ts, select = -OUTPUT)
# pred <- predict(model, pp$TS, type = 'class') ## 输出必须为factor格式
# table(pp$TS$OUTPUT, pred)
