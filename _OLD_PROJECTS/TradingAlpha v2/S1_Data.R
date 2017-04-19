
##  S1.1  Output 用于设定预测目标
Data.Output <- function(market, method = 'ZigZag', ch = 0.00370, step = 1) { ## zigzag不重绘，但是最新数据可能为NA值
  values <- ZigZag(market[ , c('High', 'Low')], change = ch, percent = F, retrace = F, lastExtreme = T)
  values.diff <- diff(values)
  signs <- sign(values.diff < 0)
  colnames(signs) <- 'OUTPUT'
  lag(signs, -step)
}

## S1.1+ Check Output Effect
Yield <- function(market, signal, step = 1) {
  ## diff close
  close <- market$Close
  close.diff <- diff(close)
  close.diff[1] <- 0
  ## signal
  signal <- lag(signal, step)
  signal <- ifelse(signal == 1, 1, -1)
  signal[1:step] <- 0
  profit <- signal * close.diff
  cumsum(profit)
  
  # cbind(market, signal, profit, cumsum(profit))
}

## S1.2 Input 用于设定预测变量组
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
  # i.vol <- volatility(OHLC(market), n = period, calc = "yang.zhang", N = 24*252)
  # colnames(i.vol) <- 'Volatility'
  cbind(i.adx, i.ar, i.cci, i.chv, i.cmo, MACD = i.macd$MACD, i.osma, RSI = i.rsi, i.stoch, i.smi) #, i.vol)
}

## S1.3 Clean
Data.Clean <- function(input, output) {
  clean <- as.data.frame(na.omit(cbind(input, output)))
  clean$OUTPUT <- as.factor(clean$OUTPUT)
  clean
}

## S1.4 Balance 样本平衡
library(caret)
Data.Balance <- function(c.dt, b.ratio = 1.05) {
  # output.table <- table(c.dt$OUTPUT)
  # ratio <- max(output.table) / min(output.table)
  # if(ratio <= b.ratio) {
  #   c.dt <- coredata(c.dt)
  #   input <- subset(c.dt, select = -OUTPUT)
  #   output <- c.dt[, 'OUTPUT']
  # } else {
  #   data.x <- subset(c.dt, select = -OUTPUT)
  #   data.y <- as.factor(c.dt$OUTPUT)
  #   balanced.data <- upSample(x = data.x, y = data.y, yname = 'OUTPUT')
  #   input <- subset(balanced.data, select = -OUTPUT)
  #   output <- as.numeric(as.character(balanced.data$OUTPUT))
  # }
  # list(
  #   Input = input,
  #   Output = as.factor(output)
  # )
  
  output.table <- table(c.dt$OUTPUT)
  ratio <- max(output.table) / min(output.table)
  if(ratio <= b.ratio) return(c.dt)
  data.x <- subset(c.dt, select = -OUTPUT)
  data.y <- c.dt$OUTPUT
  balanced.data <- upSample(x = data.x, y = data.y, yname = 'OUTPUT')
  balanced.data
}

##  S1.5 preparing 拆分为训练组和测试组
library(rminer) ## for 'holdout'
Data.Preparing <- function(balance.dt, split.ratio = 0.8) {
  # input <- balance.dt$Input
  # output <- balance.dt$Output
  # output.split <- holdout(output, ratio = split.ratio, mode = 'random')
  # output.tr.index <- output.split$tr
  # output.ts.index <- output.split$ts
  # # trans <- preProcess(input[output.tr.index, ], method = 'spatialSign') ## 将input的变量值规范到[0, 1]或者[-1, 1] @ToDo检验其他method
  # # input.tr <- predict(trans, input[output.tr.index, ])
  # # input.ts <- predict(trans, input[output.ts.index, ])
  # input.tr <- input[output.tr.index, ]
  # input.ts <- input[output.ts.index, ]
  # output.tr <- output[output.tr.index]
  # output.ts <- output[output.ts.index]
  # list(
  #   # Trans = trans,
  #   Input.Tr = input.tr,
  #   Input.Ts = input.ts,
  #   Output.Tr = output.tr,
  #   Output.Ts = output.ts
  # )
  output <- balance.dt$OUTPUT
  output.split <- holdout(output, ratio = split.ratio, mode = 'random')
  output.tr.index <- output.split$tr
  output.ts.index <- output.split$ts
  list(
    TR = balance.dt[output.tr.index, ],
    TS = balance.dt[output.ts.index, ]
  )
}
