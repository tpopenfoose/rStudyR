require(R6)

C_model1 <- R6Class(
  classname = 'recursive partitioning trees', ## 递归分割树模型
  public = list(
    initialize = function(xts.price) {
      ## ToDo
      private$data.prepared <- private$data.prepare(xts.price)
    },
    prepared.data = function() private$data.prepared,
    
    model.build = function() {
      holdout <- private$data.holdout(private$data.prepared)
      tr <- holdout$TR
      require(rpart)
      model <- rpart(OUTPUT ~., data = tr)
      print(private$model.estimate(model, holdout$TS))
    }
    
  ),
  private = list(
    data.output = function(market, method = 'ZigZag', ch = 0.00370, step = 1) { ## zigzag不重绘，但是最新数据可能为NA值
      values <- ZigZag(market[ , c('High', 'Low')], change = ch, percent = F, retrace = F, lastExtreme = T)
      values.diff <- diff(values)
      signs <- sign(values.diff < 0)
      colnames(signs) <- 'OUTPUT'
      lag(signs, -step)
    },
    data.input = function(market, period = 16) {
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
    },
    data.clean = function(input, output) {
      clean <- as.data.frame(na.omit(cbind(input, output)))
      clean$OUTPUT <- as.factor(clean$OUTPUT)
      clean
    },
    data.balance = function(c.dt, b.ratio = 1.05) {
      require(caret)
      output.table <- table(c.dt$OUTPUT)
      ratio <- max(output.table) / min(output.table)
      if(ratio <= b.ratio) return(c.dt)
      data.x <- subset(c.dt, select = -OUTPUT)
      data.y <- c.dt$OUTPUT
      balanced.data <- upSample(x = data.x, y = data.y, yname = 'OUTPUT')
      balanced.data
    },
    data.holdout = function(balance.dt, split.ratio = 0.8) {
      require(rminer)
      output <- balance.dt$OUTPUT
      output.split <- holdout(output, ratio = split.ratio, mode = 'random')
      output.tr.index <- output.split$tr
      output.ts.index <- output.split$ts
      list(
        TR = balance.dt[output.tr.index, ],
        TS = balance.dt[output.ts.index, ]
      )
    },
    data.prepare = function(xts.price) {
      op <- private$data.output(xts.price)
      ip <- private$data.input(xts.price)
      cl <- private$data.clean(ip, op)
      ba <- private$data.balance(cl)
    },
    model.estimate = function(model, ts.data) {
      pred <- predict(model, ts.data, type = 'class') ## 输出必须为factor格式
      confusionMatrix(pred, ts.data$OUTPUT)
    },
    data.prepared = NULL,
    model = NULL
  )
)