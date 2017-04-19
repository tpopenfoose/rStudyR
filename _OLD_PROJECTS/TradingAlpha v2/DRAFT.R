# ps <- findPeaks(EURUSD.xts$Close, .001)
# vs <- findValleys(EURUSD.xts$Close, .001)
# 
# AA <- EURUSD.xts
# AA$PV <- 0
# 
# AA$PV[ps] <- 100
# AA$PV[vs] <- -100


# output <- Return.Simple(EURUSD.xts)
# 
# output <- lag(output, -1)
# head(output)
# 
# 
# input <- RSI(EURUSD.xts$Close, n = 16)
# head(input)
# 
# 
# in.ou <- cbind(input, output)
# in.ou <- na.omit(in.ou)
# in.ou
# head(in.ou)
# 
# # index <- which((in.ou$EMA - 50) < -37)
# # in.ou <- in.ou[index, ]
# 
# l.model <- lm(return.simple ~ EMA, data = in.ou)
# summary(l.model)
# 
# RSI.peaks <- in.ou[findPeaks(in.ou$EMA)]

# High.peaks <- EURUSD.xts$High[lag(findPeaks(EURUSD.xts$High, thresh = 0.00), -1)]


## { 二叉树模型
io <- cbind(ip, op)
io <- na.omit(io)
io <- as.data.frame(io)
io$OUTPUT <- as.factor(io$OUTPUT)
library(rpart)

ind = sample(2, nrow(io), replace = TRUE, prob=c(0.7, 0.3))
tr <- io[ind == 1, ]
ts <- io[ind == 2, ]

rsi.rp <- rpart(OUTPUT ~., data = tr)
printcp(rsi.rp)

pred <- predict(rsi.rp, ts, type = 'class')
table(ts$OUTPUT, pred)

library(caret)
confusionMatrix(table(ts$OUTPUT, pred))
## }







