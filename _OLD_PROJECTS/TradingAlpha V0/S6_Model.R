library(deepnet)
library(doParallel)

SAE.Model <- function(p.dt, m.hidden = c(100, 100, 100), m.activationfun = 'tanh', m.learningrate = 0.6, 
                      m.momentum = 0.5, m.learningrate_scale = 1, m.output = 'sigm', m.sae_output = 'linear', 
                      m.numepochs = 10, m.batchsize = 100, m.hidden_dropout = 0, m.visible_dropout = 0, times = 1) {
  if(times < 1) return(NULL)
  SAE <- function() {
    sae <- sae.dnn.train(
      x = p.dt$Input.Tr, y = p.dt$Output.Tr, hidden = m.hidden, activationfun = m.activationfun, learningrate = m.learningrate,
      momentum = m.momentum, learningrate_scale = m.learningrate_scale, output = m.output, sae_output = m.sae_output,
      numepochs = m.numepochs, batchsize = m.batchsize, hidden_dropout = m.hidden_dropout, visible_dropout = m.visible_dropout
    )
    sae.predict <- nn.predict(sae, p.dt$Input.Ts)
    input.pred <- ifelse(sae.predict >= mean(sae.predict), 1, 0) ## ToDo 添加阈值模式
    acc <- unname(caret::confusionMatrix(p.dt$Output.Ts, input.pred)$overall['Accuracy'])
    err <- nn.test(sae, p.dt$Input.Ts, p.dt$Output.Ts, mean(sae.predict))
    list(
      SAE = sae,
      Accuracy = acc,
      Error = err
    )
  }
  if(times == 1) {
    saes <- list(
      SAE()
    ) 
  } else {
    cl <- ClusterPusk()
    saes <- foreach(times(times), .packages = 'deepnet') %dopar%
      SAE()
    stopCluster(cl)
  }

  list(
    Models = saes,
    Prepr = p.dt$Prepr
  )
}

ClusterPusk <- function() {
  ncore <- detectCores()
  cl <- makePSOCKcluster(ncore)
  registerDoParallel(cl)
  clusterSetRNGStream(cl)
  cl
}
# SAE.Model <- function(p.dt, m.hidden = c(1), m.activationfun = 'tanh', m.learningrate = 0.8, 
#                       m.momentum = 0.5, m.learningrate_scale = 1, m.output = 'sigm', m.sae_output = 'linear', 
#                       m.numepochs = 3, m.batchsize = 100, m.hidden_dropout = 0, m.visible_dropout = 0) {
#   time <- system.time(
#     sae <- sae.dnn.train(
#       x = p.dt$Input.Tr, y = p.dt$Output.Tr, hidden = m.hidden, activationfun = m.activationfun, learningrate = m.learningrate,
#       momentum = m.momentum, learningrate_scale = m.learningrate_scale, output = m.output, sae_output = m.sae_output,
#       numepochs = m.numepochs, batchsize = m.batchsize, hidden_dropout = m.hidden_dropout, visible_dropout = m.visible_dropout)
#   )
#   list(
#     Time = time,
#     SAE = sae
#   )
# }


# Model.SAE <- function(p.dt) { ## sell: -1 版本
#   
#   time <- system.time(
#     sae <- sae.dnn.train(
#       x = p.dt$Input.Tr, y = p.dt$Output.Tr, hidden = c(100, 100, 100), activationfun = "tanh", learningrate = 0.6, momentum = 0.5,
#       learningrate_scale = 1.0, output = "sigm", sae_output = "linear", numepochs = 10, batchsize = 100, hidden_dropout = 0,
#       visible_dropout = 0)
#   )
#   sae.pred <- nn.predict(sae, p.dt$Input.Ts) ## package:deepnet
#   sae.pred.mean <- mean(sae.pred)
#   pred <- ifelse(sae.pred > sae.pred.mean, 1, -1)
#   
#   cm <- confusionMatrix(p.dt$Output.Ts, pred) ## 混淆矩阵
#   
#   list(
#     Time = time,
#     SAE = sae,
#     SAE.Predict = sae.pred,
#     CM = cm
#   )
#   # return(SAE)
# }

# Model.SAE <- function(p.dt) { ## sell: 0 版本
#   time <- system.time(
#     sae <- sae.dnn.train(
#       x = p.dt$Input.Tr, y = p.dt$Output.Tr, hidden = c(100, 100, 100), activationfun = "tanh", learningrate = 0.6, momentum = 0.5,
#       learningrate_scale = 1.0, output = "sigm", sae_output = "linear", numepochs = 10, batchsize = 100, hidden_dropout = 0,
#       visible_dropout = 0)
#   )
#   sae.pred <- nn.predict(sae, p.dt$Input.Ts) ## package:deepnet
#   sae.pred.mean <- mean(sae.pred)
# # print(summary(sae.pred))
#   pred <- ifelse(sae.pred > sae.pred.mean, 1, 0)
#   cm <- confusionMatrix(p.dt$Output.Ts, pred) ## 混淆矩阵
# # print(cm)
#   list(
#     Time = time,
#     SAE = sae,
#     SAE.Predict = sae.pred,
#     CM = cm
#   )
#   # data.frame(
#   #   p.dt$Output.Ts,
#   #   pred
#   # )
# }