#--------------------	导入数据并计算所需价格值
FileToXTS <- function(file) {
  csvData <- read.csv(file,header = F)
  dateTimeString <- paste(as.character(csvData$V1),as.character(csvData$V2))
  dateTime <- as.POSIXlt(strptime(dateTimeString,"%Y.%m.%d %H:%M"))
  xtsData <- as.xts(read.zoo(cbind(dateTime,csvData[,3:7]),header=T))
  colnames(xtsData) <- c('Open','High','Low','Close','Volume')
  return(xtsData)
}
#--------------------	添加Med,CO到价格序列
AddMedCO <- function(xtsPrice) {
  Med <- with(xtsPrice,(High+Low)*0.5)
  CO <- with(xtsPrice,(Close-Open)*0.5)
  colnames(Med) <- 'Med'
  colnames(CO) <- 'CO'
  cbind(xtsPrice[,1:4],Med,CO,xtsPrice[,5])
}
#--------------------	计算输入值，基于指标组
InputValues <- function(price,p = 16) { #这里可以考虑定制MT4指标函数，再使用
  adx <- ADX(price, n = p)
  ar <- aroon(price[ ,c('High', 'Low')], n=p)[ ,'oscillator']
  colnames(ar) <- 'Aroon'
  cci <- CCI(price[ ,2:4], n = p)
  colnames(cci) <- 'CCI'
  chv <- chaikinVolatility(price[ ,2:4], n = p)
  colnames(chv) <- 'ChainKinV'
  cmo <- CMO(price[ ,'Med'], n = p)
  colnames(cmo) <- 'CMO'
  macd <- MACD(price[ ,'Med'], 12, 26, 9)[ ,'macd']
  osma <- macd - MACD(price[ ,'Med'],12, 26, 9)[ ,'signal']
  colnames(osma) <- 'OSMA'
  rsi <- RSI(price[ ,'Med'], n = p)
  colnames(rsi) <- 'RSI'
  stoh <- stoch(price[ ,2:4],14, 3, 3)
  smi <- SMI(price[ ,2:4],n = p, nFast = 2, nSlow = 25, nSig = 9)
  colnames(smi) <- c('SMI','SMIS')
  vol <- volatility(price[ ,1:4],n = p,calc="yang.zhang", N=96)
  colnames(vol) <- 'Volatility'
  cbind(adx, ar, cci, chv, cmo, macd, osma, rsi, stoh, smi, vol)
}
#--------------------	计算输出值，基于ZigZag指标
OutputValues <- function(price,ch = 0.00370) {
  ZigZigValues <- ZigZag(price[ ,'Med'], change = ch, percent = F, retrace = F, lastExtreme = T)
  ZigZigValues <- as.vector(ZigZigValues)
  output <- c(diff(ZigZigValues),NA)
  output[which(output > 0)] <- 0
  output[which(output < 0)] <- 1
  return(output)
}
#--------------------	样本平衡
Balancing <- function(data) {
  outputs <- table(data$OUTPUT)
  ratio <- max(outputs)/min(outputs)
  if(ratio <= 1.05) return(data)
  BalancedData <- upSample(x = subset(data, select = -OUTPUT), y = as.factor(subset(data, select = OUTPUT)), yname = 'OUTPUT')
  BalancedData$OUTPUT <- as.numeric(BalancedData$OUTPUT)-1
  BalancedData <- as.matrix(BalancedData)
  return(BalancedData)
}
#--------------------	深度学习整合函数
DeepLearning <- function(price, r = 8/10, m = "random", norm = "spatialSign", h = c(10), act = "tanh", LR = 0.8, Mom = 0.5, 
                         out = "sigm", sae = "linear", Ep = 10, Bs = 50, bar = 500, dec = 1, ans = 4) {
  # 产生所需价格相关值
  PRICE <- AddMedCO(price)
  # 生成输入和输出值
  INPUT <- InputValues(PRICE)
  OUTPUT <- OutputValues(PRICE)
  # 合并输入和输出值
  TABLE <- cbind(INPUT,OUTPUT = OUTPUT)
  # 清洗输入输出值
  TABLE <- na.omit(TABLE)
  # 平衡输入输出值
  TABLE.B <- Balancing(TABLE)
  X <- subset(TABLE.B, select = -OUTPUT)
  Y <- subset(TABLE.B, select = OUTPUT)
  
  # 训练和测试数据集的索引，可用set.seed固定结果
  T <- holdout(Y, ratio = r, mode = m)
  # 预处理参数
  PREPROCESS <- preProcess(X[T$tr, ], method = norm)
  # 预处理分为训练和测试数据集
  X.TR <- predict(PREPROCESS, X[T$tr, ]) # 输入训练集
  X.TS <- predict(PREPROCESS, X[T$ts, ]) # 输入测试集
  Y.TR <- Y[T$tr] # 输出训练集
  Y.TS <- Y[T$ts] # 输入测试集
  # 训练模型
  cl <- CreateCluster()
  SAE <- foreach(times(ans), .packages = "deepnet") %dopar%
    sae.dnn.train(x = X.TR, y = Y.TR, hidden = h, activationfun = act, learningrate = LR, momentum = Mom, output = out,
                  sae_output = sae, numepochs = Ep, batchsize = Bs)
  stopCluster(cl)
  #	对测试数据集进行验证
  PR.SAE <- (foreach(i = 1:ans, .combine = "+") %do% nn.predict(SAE[[i]], X.TS))/ans
  if(dec == 1)	PR <- ifelse(PR.SAE > mean(PR.SAE), 1, 0)
  if(dec == 2)	PR <- ifelse(PR.SAE > 0.6, -1, ifelse(PR.SAE < 0.4, 1, 0))
  ACCURACY <- confusionMatrix(Y.TS, PR)
  #ERROR <- nn.test(SAE, X.TS, Y.TS, mean(PR.SAE))
  ERROR <- (foreach(i = 1:ans, .combine = "+") %do% nn.test(SAE[[i]], X.TS, Y.TS, mean(PR.SAE)))/ans
  # 检验资金变换
  X <- subset(TABLE, select = -OUTPUT)
  Y <- subset(TABLE, select = OUTPUT)
  X.TS <- predict(PREPROCESS, tail(X, bar))
  Y.TS <- tail(Y, bar)
  PR.SAE <- (foreach(i = 1:ans, .combine = "+") %do% nn.predict(SAE[[i]], X.TS))/ans
  if(dec == 1) sig <- ifelse(PR.SAE  > mean(PR.SAE ), -1, 1)
  if(dec == 2) sig <- ifelse(PR.SAE  > 0.6, -1, ifelse(PR.SAE  < 0.4, 1, 0))
  BALANCE <- cumsum(tail(PRICE[, 'CO'], bar) * sig) # 训练余额曲线
  sig.zz <- ifelse(Y.TS == 0, 1, -1)
  BALANCE.ZZ <- cumsum(tail(PRICE[, 'CO'], bar) * sig.zz) # 原ZigZag指标余额曲线
  # 生成输出结果
  result <- list(PREPROCESS = PREPROCESS, SAE = SAE, TABLE = TABLE, ACCURACY = ACCURACY, ERROR = ERROR, BALANCE = BALANCE,
                 BALANCE.ZZ = BALANCE.ZZ)
  return(result)
}
#--------------------	误差/精度系数 X为input，Y为output
Estimation <- function(X, Y, r = 8/10, m = "random", norm = "spatialSign", h = c(10), act = "tanh", LR = 0.8, Mom = 0.5,
                       out = "sigm", sae = "linear", Ep = 10, Bs = 50, CM = F) { # CM为T时返回精度；F时返回误差
  #Indices of the training and test data set
  t <- holdout(Y, ratio = r, mode = m)
  #Parameters of preprocessing
  prepr <- preProcess(X[t$tr, ], method = norm)
  #Divide into train and test data sets with preprocessing
  x.tr <- predict(prepr, X[t$tr, ])
  x.ts <- predict(prepr, X[t$ts, ])
  y.tr <- Y[t$tr]
  y.ts <- Y[t$ts]
  #Train the model
  SAE <- sae.dnn.train(x = x.tr , y = y.tr , hidden = h,activationfun = act,learningrate = LR, momentum = Mom, 
                       output = out, sae_output = sae, numepochs = Ep, batchsize = Bs)
  #Obtain a forecast on the test data set
  pr.sae <- nn.predict(SAE, x.ts)
  #Recode it into signals 1,0
  pr <- ifelse(pr.sae > mean(pr.sae), 1, 0)
  #Calculate the Accuracy coefficient or classification error
  if(CM) err<-unname(confusionMatrix(y.ts, pr)$overall[1])
  if(!CM) err<-nn.test(SAE, x.ts, y.ts, mean(pr.sae))
  return(err)
}
#--------------------	测试余额水平 dt1为测试样本，dt2为原样本
Testing <- function(dt1, dt2, r = 8/10, m = "random", norm = "spatialSign", h = c(10), act = "tanh", LR = 0.8, Mom = 0.5,
                    out = "sigm", sae = "linear", Ep = 10, Bs=50, pr = T, bar = 500) {
  X <- dt1[, -ncol(dt1)]
  Y <- dt1[, ncol(dt1)]
  t <- holdout(Y, ratio = r, mode = m)
  prepr <- preProcess(X[t$tr, ], method = norm)
  x.tr <- predict(prepr, X[t$tr, ])
  y.tr <- Y[t$tr]
  SAE <- sae.dnn.train(x = x.tr , y = y.tr , hidden = h, activationfun = act, learningrate = LR, momentum = Mom,
                       output = out, sae_output = sae, numepochs = Ep, batchsize = Bs)
  X <- dt2[ ,-ncol(dt2)]
  Y <- dt2[ ,ncol(dt2)]
  x.ts <- predict(prepr, tail(X, bar))
  y.ts <- tail(Y, bar)
  pr.sae <- nn.predict(SAE, x.ts)
  sig <- ifelse(pr.sae > mean(pr.sae), -1, 1)
  sig.zz <- ifelse(y.ts == 0, 1, -1)
  bal <- cumsum(tail(price[, 'CO'], bar) * sig)
  bal.zz <- cumsum(tail(price[, 'CO'], bar) * sig.zz)
  if(pr) return(bal)
  if(!pr) return(bal.zz)
}
#--------------------	测试余额水平1 dt1为测试样本，dt2为原样本；增加decode方式
Testing.1 <- function(dt1, dt2, r = 8/10, m = "random", norm = "spatialSign", h = c(10), act = "tanh", LR = 0.8, Mom = 0.5,
                    out = "sigm", sae = "linear", Ep = 10, Bs=50, pr = T, bar = 500, dec = 1) {
  X <- dt1[, -ncol(dt1)]
  Y <- dt1[, ncol(dt1)]
  t <- holdout(Y, ratio = r, mode = m)
  prepr <- preProcess(X[t$tr, ], method = norm)
  x.tr <- predict(prepr, X[t$tr, ])
  y.tr <- Y[t$tr]
  SAE <- sae.dnn.train(x = x.tr , y = y.tr , hidden = h, activationfun = act, learningrate = LR, momentum = Mom,
                       output = out, sae_output = sae, numepochs = Ep, batchsize = Bs)
  X <- dt2[ ,-ncol(dt2)]
  Y <- dt2[ ,ncol(dt2)]
  x.ts <- predict(prepr, tail(X, bar))
  y.ts <- tail(Y, bar)
  pr.sae <- nn.predict(SAE, x.ts)
  if(dec == 1) sig <- ifelse(pr.sae > mean(pr.sae), -1, 1)
  if(dec == 2) sig <- ifelse(pr.sae > 0.6, -1, ifelse(pr.sae < 0.4, 1, 0))
  sig.zz <- ifelse(y.ts == 0, 1, -1)
  bal <- cumsum(tail(price[, 'CO'], bar) * sig)
  bal.zz <- cumsum(tail(price[, 'CO'], bar) * sig.zz)
  if(pr) return(bal)
  if(!pr) return(bal.zz)
}
#--------------------	测试余额水平2 dt1为测试样本，dt2为原样本；增加decode方式，并行
Testing.2 <- function(dt1, dt2, r = 8/10, m = "random", norm = "spatialSign", h = c(10), act = "tanh", LR = 0.8, Mom = 0.5,
                      out = "sigm", sae = "linear", Ep = 10, Bs=50, pr = T, bar = 500, dec = 1, ans = 4) {
  X <- dt1[, -ncol(dt1)]
  Y <- dt1[, ncol(dt1)]
  t <- holdout(Y, ratio = r, mode = m)
  prepr <- preProcess(X[t$tr, ], method = norm)
  x.tr <- predict(prepr, X[t$tr, ])
  y.tr <- Y[t$tr]
  cl <- CreateCluster()
  SAE <- foreach(times(ans), .packages = "deepnet") %dopar%
    sae.dnn.train(x = x.tr, y = y.tr, hidden = h, activationfun = act, learningrate = LR, momentum = Mom,
                  output = out, sae_output = sae, numepochs = Ep, batchsize = Bs)
  stopCluster(cl)
  #SAE <- sae.dnn.train(x = x.tr , y = y.tr , hidden = h, activationfun = act, learningrate = LR, momentum = Mom,
  #                     output = out, sae_output = sae, numepochs = Ep, batchsize = Bs)
  X <- dt2[ ,-ncol(dt2)]
  Y <- dt2[ ,ncol(dt2)]
  x.ts <- predict(prepr, tail(X, bar))
  y.ts <- tail(Y, bar)
  pr.sae <- (foreach(i = 1:ans, .combine = "+") %do%
               nn.predict(SAE[[i]], x.ts))/ans
  #pr.sae <- nn.predict(SAE, x.ts)
  if(dec == 1) sig <- ifelse(pr.sae > mean(pr.sae), -1, 1)
  if(dec == 2) sig <- ifelse(pr.sae > 0.6, -1, ifelse(pr.sae < 0.4, 1, 0))
  sig.zz <- ifelse(y.ts == 0, 1, -1)
  bal <- cumsum(tail(PRICE[, 'CO'], bar) * sig)
  bal.zz <- cumsum(tail(PRICE[, 'CO'], bar) * sig.zz)
  if(pr) return(bal)
  if(!pr) return(bal.zz)
}
#--------------------	创建并行套接字集群
CreateCluster <- function() {
  cores <- detectCores()
  cl <- makePSOCKcluster(cores)
  registerDoParallel(cl)
  #clusterSetRNGStream(cl)
  return(cl)
}