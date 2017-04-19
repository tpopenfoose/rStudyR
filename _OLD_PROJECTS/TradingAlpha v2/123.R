source('C:/Users/admin/AppData/Roaming/MetaQuotes/Terminal/67742D7BC090B0FB76C057D87D28E159/MQL4/Experts/ACCRETION2016/R/SAE2016 v1.R', encoding = 'UTF-8')

rm(list = ls())

library(quantmod)
library(deepnet)
library(rminer)

#-------------------- MT4 History Data into XTS form
MT4DataToXTS <- function(symbol, period, time, open, high, low, close) {
  xts(cbind(Open = open, High = high, Low = low, Close = close), as.POSIXct(time, origin = '1970-01-01', tz = 'GMT'))
  # # 检查变量是否已经存在，T：合并; F：返回
  # symbol <- toupper(symbol)
  # varName <- paste('PRICE', symbol, period, sep = '_')
  # if(!exists(varName)) return(newData)
  # rbind.xts(get(varname), newData)
}

Data.Input <- function(market, period = 16) {
  market.HLC <- HLC(market)
  market.C <- Cl(market)
  i.adx <- ADX(market.HLC, n = period)
  i.ar <- aroon(market[ ,c('High', 'Low')], n = period)$oscillator
  colnames(i.ar) <- 'Aroon'
  i.cci <- CCI(market.HLC, n = period)
  colnames(i.cci) <- 'CCI'
  i.chv <- chaikinVolatility(market.HLC, n = period)
  colnames(i.chv) <- 'CK.Volatility'
  i.cmo <- CMO(market.C, n = period)
  colnames(i.cmo) <- 'CMO'
  i.macd <- MACD(market.C, nFast = 12, nSlow = 26, nSig = 9)
  colnames(i.macd) <- c('MACD', 'MACDS')
  i.osma <- i.macd$MACD - i.macd$MACDS
  colnames(i.osma) <- 'OSMA'
  i.rsi <- RSI(market.C, n = period)
  colnames(i.rsi) <- 'RSI'
  i.stoch <- stoch(market.HLC, nFastK = 14, nFastD = 3, nSlowD = 3)
  colnames(i.stoch) <- c('STO.FK', 'STO.FD', 'STO.SD')
  i.smi <- SMI(market.HLC, n = period, nFast = 2, nSlow = 25, nSig = 9)
  colnames(i.smi) <- c('SMI','SMIS')
  na.omit(cbind(i.adx, i.ar, i.cci, i.chv, i.cmo, MACD = i.macd$MACD, i.osma, RSI = i.rsi, i.stoch, i.smi))
}

GetSignals <- function(model, new.input, bars = 0) {
  rows <- nrow(new.input)
  new.input <- as.data.frame(new.input)
  if(bars > rows | bars == 0) bars <- rows
  
  # input <- subset(data.clean, select = -OUTPUT)
  # output <- data.clean[, 'OUTPUT']
  # print(input)
  trans <- model$Trans
  
  input.ts <- tail(new.input, bars)
  # print(input.ts)
  input.ts <- predict(trans, input.ts)
  # output.ts <- tail(output, bars)
  # print(input.ts)
  # AAA <<- input.ts
  # do.call(nn.predict, list(model$Models, input.ts))
  input.pred <- nn.predict(model$Models[[1]][[1]], coredata(input.ts))
  input.pred.mean <- mean(input.pred)
  sig <- ifelse(input.pred > input.pred.mean, 1, -1)
  as.numeric(tail(sig, 1))
  
  
  # as.numeric(tail(input.pred, 1))
  
  # input.pred
  # # print(input.pred.mean) 
  # # sig <- ifelse(input.pred > 0.5, 1, 0)
  # 
  # # print(head(sig))
  # # sigX <- sig
  # # sigY <-output.ts
  # # SIG <<- cbind(sigY, sigX)
  # # print(confusionMatrix(output.ts, sig))
  # sig.x <- ifelse(sig == 1, 1, -1)
  # sig.x <- xts(sig.x, index(input.ts))
  # colnames(sig.x) <- 'SIG'
  # 
  # diff.close <- diff(t.EURUSD$Close)
  # colnames(diff.close) <- 'Diff'
  # diff.close[1] <- 0
  # 
  # profit <- diff.close * lag(sig.x, 1)
  # profit[1] <- 0
  # colnames(profit) <- 'Profit'
  # 
  # equity <- cumsum(profit)
  # colnames(equity) <- 'Equity'
  # 
  # result <- cbind(t.EURUSD, diff.close, sig.x, profit, equity)
  # result <- na.omit(result)
  # 
}