
library(magrittr)
library(data.table)
library(TTR)
library(caret)
library(rminer)
library(randomUniformForest)

# price.factors <- function(price) {
#   price[, .(OPEN, HIGH, LOW, CLOSE, MEDIAN = (HIGH + LOW) * 0.5)]
# }
price.data.add.median <- function(price.data) {
  price.data[, MEDIAN := (HIGH + LOW) * 0.5]
}

fun.input.default <- function(price, period=16) {
  adx <- ADX(price[, .(HIGH, LOW, CLOSE)], n = period) %>% as.data.table %>%
    extract(
      j = .(DX, ADX, oscDX = DIp - DIn)
    )
  aroon <- aroon(price[, .(HIGH, LOW)], n = period) %>% as.data.table %>% extract(j = .(oscillator))
  atr <- ATR(price[, .(HIGH, LOW, CLOSE)], n = period, maType = 'EMA') %>% as.data.table %>%
    extract(j = c(1:2), with = FALSE)
  cci <- CCI(price[, .(HIGH, LOW, CLOSE)], n = period)
  chv <- chaikinVolatility(price[, .(HIGH, LOW)], n = period)
  cmo <- CMO(price[, MEDIAN], n = period)
  macd <- MACD(price[, MEDIAN], 12, 26, 9) %>% as.data.table %>% extract(
    j = .(sign = signal, vsig = signal %>% diff %>% c(NA,.) %>% multiply_by(10))
  )
  rsi <- RSI(price[, MEDIAN], n = period)
  stoh <- stoch(price[, .(HIGH, LOW, CLOSE)], nFastK = period, nFastD =3, nSlowD = 3,
                maType = "EMA")%>% as.data.table %>%
    extract(
      j = .(slowD, oscK = fastK - fastD)
    )
  smi <- SMI(price[, .(HIGH, LOW, CLOSE)],n = period, nFast = 2, nSlow = 25, nSig = 9)
  vol <- volatility(price[, .(OPEN, HIGH, LOW, CLOSE)], n = period, calc = "yang.zhang", N = 144)
  cbind(adx, aroon, atr, cci, chv, cmo, macd, rsi, stoh, smi, vol)
}

fun.output.default <- function(price, change = 0.0050, mode=c('median', 'high.low', 'close')) {
  using.price <- switch(
    match.arg(mode),
    'median' = price[, MEDIAN],
    'high.low' = price[, .(HIGH, LOW)],
    'close' = price[, CLOSE]
  )
  zz <- ZigZag(using.price, change = ch, percent = FALSE, retrace = FALSE, lastExtreme = TRUE)
  sig <- zz %>% diff %>% c(0, .) %>% sign
  # cbind(zz, sig)
}

data.clean <- function(input.table, output.table) {
  cbind(input, OUTPUT = output.table %>% as.factor) %>% na.omit
}

input.cutoff <- function(cleaned.data, cutoff=0.9) {
  high.correlation <- 
    cleaned.data[, !'OUTPUT'] %>%
    cor %>%
    caret::findCorrelation(cutoff = cutoff)
  cleaned.data[, !high.correlation, with = FALSE]
}

data.balance <- function(cleaned.data, balance.ratio.threshold=1.05) {
  output <- table(cleaned.data$OUTPUT)
  ratio <- max(output) / min(output)
  if(ratio <= balance.ratio.threshold) {
    cleaned.data
  } else {
    caret::upSample(x = cleaned.data[, !'OUTPUT'], y = cleaned.data[, OUTPUT], yname = 'OUTPUT')
  }
}




best.importance <- function(balanced.data, holdout.ratio=2/3, holdout.mode='stratified',
                            pre.process.mode=c("center", "spatialSign"),
                            mtry=1, ntree=300, nodesize=1, threads='auto',
                            nbest=10, npar) {
  index <- rminer::holdout(y = balanced.data[, OUTPUT], ratio = holdout.ratio, mode = holdout.mode)
  train.input <- balanced.data[index$tr, !'OUTPUT']
  test.input <- balanced.data[index$ts, !'OUTPUT']
  train.output <- balanced.data[index$tr, OUTPUT]
  test.output <- balanced.data[index$ts, OUTPUT]
  pre.process <- caret::preProcess(train.input, method = pre.process.mode)
  train.input %<>% predict(pre.process, .)
  test.input %<>% predict(pre.process, .)
  random.uniform.forest <- randomUniformForest(
    X = train.input,
    Y = train.output,
    xtest = test.input,
    ytest = test.output,
    mtry = mtry,
    ntree = mtree,
    nodesize = nodesize,
    threads = threads
  )
  random.uniform.forest.importance <- importance(random.uniform.forest, Xtest=test.input)
  best.importance <-
    random.uniform.forest.importance$localVariableImportance$classVariableImportance %>%
    head(10) %>% rownames
  
  best.sell.importance <- partialImportance(X = test.input, random.uniform.forest.importance,
                                            whichClass = "-1", nLocalFeatures = 7) %>%
    row.names %>% as.numeric %>% colnames(test.input)[.]
  
  best.buy.importance <- partialImportance(X = test.input, random.uniform.forest.importance,
                                           whichClass = "1", nLocalFeatures = 7) %>% 
    row.names %>% as.numeric %>% colnames(test.input)[.]
  list(
    BEST = best.importance,
    BEST.BUY = best.buy.importance,
    BEST.SELL = best.sell.importance
  )
}








#### file ####
model.file.path <- function(model.file.dir, symbol, timeframe, file.extension) {
  sprintf('%s_%s.%s', symbol, timeframe, file.extension) %>%
    file.path(model.file.dir, .)
}






