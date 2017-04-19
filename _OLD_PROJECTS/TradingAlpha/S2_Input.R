
Input <- function(market, period = 16) {
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