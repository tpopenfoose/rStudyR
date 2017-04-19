
##  S2.1  Preprocessing
library(rminer) ## for 'holdout'

Model.Preprocessing <- function(balance.dt, split.ratio = 0.8) {
  input <- balance.dt$Input
  output <- balance.dt$Output
  output.split <- holdout(output, ratio = split.ratio, mode = 'random')
  output.tr.index <- output.split$tr
  output.ts.index <- output.split$ts
  trans <- preProcess(input[output.tr.index, ], method = 'spatialSign') ## 将input的变量值规范到[0, 1]或者[-1, 1] @ToDo检验其他method
  input.tr <- predict(trans, input[output.tr.index, ])
  input.ts <- predict(trans, input[output.ts.index, ])

  list(
    Trans = trans,
    Input.Tr = input.tr,
    Input.Ts = input.ts,
    Output.Tr = output[output.tr.index],
    Output.Ts = output[output.ts.index]
  )
}

##  S2.2  Building
library(deepnet) ## for 'sae.dnn.train'

Model.Building <- function(dt, m.hidden = c(100, 100, 100), m.activationfun = 'tanh', m.learningrate = 0.6, 
                           m.momentum = 0.5, m.learningrate_scale = 1, m.output = 'sigm', m.sae_output = 'linear', 
                           m.numepochs = 10, m.batchsize = 100, m.hidden_dropout = 0, m.visible_dropout = 0, times = 1) {
  if(times < 1) return(NULL)
  SAE <- function() {
    sae <- sae.dnn.train(
      x = as.matrix(dt$Input.Tr), y = dt$Output.Tr, hidden = m.hidden, activationfun = m.activationfun, learningrate = m.learningrate,
      momentum = m.momentum, learningrate_scale = m.learningrate_scale, output = m.output, sae_output = m.sae_output,
      numepochs = m.numepochs, batchsize = m.batchsize, hidden_dropout = m.hidden_dropout, visible_dropout = m.visible_dropout
    )
    # sae <- sae.dnn.train(
    #   x = dt$Input.Tr, y = dt$Output.Tr, hidden = m.hidden, activationfun = m.activationfun, learningrate = m.learningrate,
    #   momentum = m.momentum, learningrate_scale = m.learningrate_scale, output = m.output, sae_output = m.sae_output,
    #   numepochs = m.numepochs, batchsize = m.batchsize, hidden_dropout = m.hidden_dropout, visible_dropout = m.visible_dropout
    # )
    sae.predict <- nn.predict(sae, dt$Input.Ts)
    # input.pred <- ifelse(sae.predict >= mean(sae.predict), 1, 0) ## ToDo 添加阈值模式
    input.pred <- ifelse(sae.predict >= 0.5, 1, 0)
    
# print(summary(sae.predict))
# print(sae.predict)
# print(cbind(input.pred, dt$Output.Ts))
    acc <- unname(caret::confusionMatrix(dt$Output.Ts, input.pred)$overall['Accuracy'])
    err <- nn.test(sae, dt$Input.Ts, dt$Output.Ts, mean(sae.predict))
    list(
      SAE = sae,
      Accuracy = acc,
      Error = err
    )
  }
  if(times == 1) saes <- list(SAE()) 
  else {
    cl <- ClusterPusk()
    saes <- foreach(times(times), .packages = 'deepnet') %dopar% SAE()
    stopCluster(cl)
  }
  list(
    Models = saes,
    Trans = dt$Trans
  )
}

StartPusk <- function() {
  ncore <- detectCores()
  cl <- makePSOCKcluster(ncore)
  registerDoParallel(cl)
  clusterSetRNGStream(cl)
  cl
}



##  S2.3  Model Testing

Model.Testing <- function(model, data.clean, bars = 0) {
  rows <- nrow(data.clean)
  if(bars > rows | bars == 0) bars <- rows
  
  input <- subset(data.clean, select = -OUTPUT)
  output <- data.clean[, 'OUTPUT']
# print(input)
  trans <- model$Trans
  
  input.ts <- tail(input, bars)
# print(input.ts)
  input.ts <- predict(trans, input.ts)
  output.ts <- tail(output, bars)
  # print(input.ts)
  # sae.pred <- nn.predict()
  # do.call(nn.predict, list(model$Models, input.ts))
  input.pred <- nn.predict(model$Models[[1]][[1]], coredata(input.ts))
  
  
#   input.pred.mean <- mean(input.pred)
#   sig <- ifelse(input.pred > input.pred.mean, 1, 0)
# # print(input.pred.mean)
#   # sig <- ifelse(input.pred > 0.5, 1, 0)
# 
#   # print(head(sig))
#   # sigX <- sig
#   # sigY <-output.ts
#   # SIG <<- cbind(sigY, sigX)
# # print(confusionMatrix(output.ts, sig))
#   sig.x <- ifelse(sig == 1, 1, -1)
#   sig.x <- xts(sig.x, index(input.ts))
#   colnames(sig.x) <- 'SIG'
# 
#   # diff.close <- diff(t.EURUSD$Close)
#   diff.close <- diff(t.EURUSD$Close)
#   colnames(diff.close) <- 'Diff'
#   diff.close[1] <- 0
# 
#   profit <- diff.close * lag(sig.x, 1)
#   profit[1] <- 0
#   colnames(profit) <- 'Profit'
# 
#   equity <- cumsum(profit)
#   colnames(equity) <- 'Equity'
# 
#   # result <- cbind(t.EURUSD, diff.close, sig.x, profit, equity)
#   result <- cbind(t.EURUSD, diff.close, sig.x, profit, equity)
#   result <- na.omit(result)

}