
#### functions
Data.Output <- function(market, step = 1) {  ## sell: 0 版本 ## 可扩展，添加method或者args
  values.diff <- with(market, Close - Open)
  output <- ifelse(values.diff > 0, 10, 20) ## B:10, S:20
  colnames(output) <- 'OUTPUT'
  lag(output, -step)
  # output$OUTPUT <- as.factor(output$OUTPUT)
  # output
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
  cbind(i.adx, i.ar, i.cci, i.chv, i.cmo, MACD = i.macd$MACD, i.osma, RSI = i.rsi, i.stoch, i.smi)
}

Data.Clean <- function(input, output) {
  na.omit(cbind(input, output))
}

library(caret)

Data.Balance <- function(c.dt, b.ratio = 1.05) {
print(c.dt$OUTPUT)
  output.table <- table(c.dt$OUTPUT)
print(output.table)
  ratio <- max(output.table) / min(output.table)
  if(ratio <= b.ratio) {
    c.dt <- coredata(c.dt)
    input <- subset(c.dt, select = -OUTPUT)
    output <- c.dt[, 'OUTPUT']
  } else {
    data.x <- subset(c.dt, select = -OUTPUT)
    data.y <- as.factor(c.dt$OUTPUT)
    balanced.data <- upSample(x = data.x, y = data.y, yname = 'OUTPUT')
    input <- subset(balanced.data, select = -OUTPUT)
    output <- as.numeric(as.character(balanced.data$OUTPUT))
  }
  list(
    Input = input,
    Output = output
  )
}
