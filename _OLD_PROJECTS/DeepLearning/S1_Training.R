source('S0_InputData.R')
library(deepnet)

t1 <- system.time(SAE <- sae.dnn.train(x= X.TRAINING, y = Y[T$tr], hidden=c(100,100,100), activationfun = "tanh", 
                                 learningrate = 0.6, momentum = 0.5, learningrate_scale = 1.0, output = "sigm", 
                                 sae_output = "linear", numepochs = 10, batchsize = 100, hidden_dropout = 0, visible_dropout = 0))
# 在测试集上进行评估
PREDICT.SAE <- nn.predict(SAE, X.TEST)
print(summary(PREDICT.SAE))# 自动化可以删除此行

PREDICT.MEAN <- mean(PREDICT.SAE)
PREDICT <- ifelse(PREDICT.SAE > PREDICT.MEAN, 1, 0)

EVALUATION <- confusionMatrix(Y[T$ts], PREDICT)
print(EVALUATION) # 打印测试结果

#subset(FUNCTIONVALUE,select = -ZigZagV)

X.NEW <- predict(spSign,tail(subset(FUNCTIONVALUE,select = -ZigZagV), 500))
PREDICT.SAE.NEW <- nn.predict(SAE,X.NEW)
PREDICT.SIGNAL <- ifelse(PREDICT.SAE.NEW > mean(PREDICT.SAE.NEW), -1, 1)

Y.NEW <- ifelse(tail(subset(FUNCTIONVALUE,select = ZigZagV), 500) == 0, 1, -1)
CM.NEW <- confusionMatrix(Y.NEW, PREDICT.SIGNAL)
print(CM.NEW)

BALANCE <- cumsum(tail(PRICE[,'CO'],500)*PREDICT.SIGNAL)
plot(BALANCE, t = 'l')
abline(h=0)

BALANCE.ZZ <- cumsum(tail(PRICE[,'CO'],500)*Y.NEW)
plot(as.vector(BALANCE.ZZ), t = 'l')
lines(as.vector(BALANCE),col = 2)