library(DMwR)

# trade

# library(PerformanceAnalytics)
diffprice <- Simulator(t.EURUSD)

pred <- Output(t.EURUSD)
pred <- ifelse(pred == 1, 1, -1)


profit <- pred * diffprice

profit <- na.omit(profit)

profitline <- cumsum(profit)