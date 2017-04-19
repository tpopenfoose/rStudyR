##### Source List
## Books: 
## 1. Introduction to R for Quantitative Finance




##### STEP 1: To xts
## 1.1 "MT4 csv file" to "xts"
library(quantmod)

MT4csvToXTS <- function(file) {
  symbol <- read.csv(file, header = F)
  time.str <- paste(symbol$V1, symbol$V2)
  xts.data <- xts(symbol[, 3:7], as.POSIXct(time.str, format = '%Y.%m.%d %H:%M'))
  colnames(xts.data) = c('Open', 'High', 'Low', 'Close', 'Volume')
  xts.data
}
load('./Data/eurusd60.rdata')

##### STEP 2: return; 
## B1 {
eurusd.c <- t.EURUSD$Close
plot(eurusd.c, main = 'EURUSD Closing', ylab = 'Close(EURUSD)', xlab = 'DateTime')
head(eurusd.c)
tail(eurusd.c)
eurusd.c[which.max(eurusd.c)]
## 简单收益率和复合收益率，可见在数字比较小的情况下，误差很小，在0.0001级别
return.simp <- diff(eurusd.c) / lag(eurusd.c, k = -1) * 100
return.simp <- na.trim(return.simp) ## 删除NA项
return.cont <- diff(log(eurusd.c)) * 100
return.cont <- na.trim(return.cont) ## 删除NA项

summary(coredata(return.simp))
return.simp[which.min(return.simp)]

hist(return.simp, breaks = 100, main = 'Histogram of Simple Returns', xlab = '%')
eurusd.c_2015 <- window(eurusd.c, start = '2015-01-01', end = '2015-12-31') ## 时间分隔
eurusd.c_2015[which.max(eurusd.c_2015)]

quantile(return.simp, probs = 0.01, na.rm = T) ## 99% 的概率日收益率高于此值，1%出现几率意味着每年2.5 * 24次(252 * 24 * 1%)
## B1 }

##### STEP 3: arima
## B1 {
library(forecast)
# frequency(eurusd.c) 原文引用的月数据，这里显示12
## Model identification and estimation 模型辨识与估计
return.simp1000 <- tail(return.simp, 1000)
mod <- auto.arima(return.simp800, stationary = TRUE, seasonal = FALSE, ic = 'aic')
mod
confint(mod) ## 置信区间
## Model diagnostic checking 模型的诊断检验
tsdiag(mod)
plot(mod$x, lty = 1, main = "UK house prices: raw data vs. fitted values", ylab = "Return in percent", xlab = "Date")
lines(fitted(mod), lty = 2,lwd = 2, col = "red")
accuracy(mod)
## Forecasting 预测
predict(mod, n.ahead = 3) ## Predict; Standard Error
plot(forecast(mod))
## B1 }

##### STEP 4: volatility for risk management
Box.test(coredata(return.simp^2), type = 'Ljung-Box', lag = 12)
library(FinTS)
ArchTest(coredata(return.simp)) ## p-value 很低表明存在arch效应

library(rugarch)
eurusd.garch11.spec <- ugarchspec(variance.model = list(garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0)))
eurusd.garch11.fit <- ugarchfit(spec = eurusd.garch11.spec, data = return.simp)
eurusd.garch11.roll <- ugarchroll(eurusd.garch11.spec, return.simp, n.start = 1000, refit.every = 1, refit.window = 'moving',
                                  solver = 'Hybird', calculate.VaR = TRUE, VaR.alpha = 0.01, keep.coef = TRUE)
report(eurusd.garch11.roll, type = 'VaR', VaR.alpha = 0.01, conf.level = 0.99)


























