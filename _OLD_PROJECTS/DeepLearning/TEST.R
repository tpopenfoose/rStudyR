PRICE <- AddMedCO(PRICE)
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

T <- holdout(Y,ratio = 8/10, mode = "random")

spSign <- preProcess(X[T$tr, ], method = "spatialSign")
X.TRAINING <- predict(spSign, X[T$tr, ])
X.TEST     <- predict(spSign, X[T$ts, ])

TIME1 <- system.time(SAE <- sae.dnn.train(x= X.TRAINING, y = Y[T$tr], hidden=c(100,100,100), activationfun = "tanh", 
                                          learningrate = 0.6, momentum = 0.5, learningrate_scale = 1.0, output = "sigm", 
                                          sae_output = "linear", numepochs = 10, batchsize = 100, hidden_dropout = 0, visible_dropout = 0))
# 训练结果
PREDICT.SAE <- nn.predict(SAE, X.TEST)
print(summary(PREDICT.SAE))# 自动化可以删除此行

PREDICT.MEAN <- mean(PREDICT.SAE)
PREDICT <- ifelse(PREDICT.SAE > PREDICT.MEAN, 1, 0)

EVALUATION <- confusionMatrix(Y[T$ts], PREDICT)
print(EVALUATION) # 打印测试结果

X.NEW <- predict(spSign,tail(subset(TABLE, select = -OUTPUT), 500))
PREDICT.SAE.NEW <- nn.predict(SAE,X.NEW)
PREDICT.SIGNAL <- ifelse(PREDICT.SAE.NEW > mean(PREDICT.SAE.NEW), -1, 1)

Y.NEW <- ifelse(tail(subset(TABLE, select = OUTPUT), 500) == 0, 1, -1)
CM.NEW <- confusionMatrix(Y.NEW, PREDICT.SIGNAL)
print(CM.NEW)

BALANCE <- cumsum(tail(PRICE[, 'CO'], 500) * PREDICT.SIGNAL)
plot(BALANCE, t = 'l')
abline(h=0)

BALANCE.ZZ <- cumsum(tail(PRICE[,'CO'], 500) * Y.NEW)
plot(as.vector(BALANCE.ZZ), t = 'l')
lines(as.vector(BALANCE),col = 2)