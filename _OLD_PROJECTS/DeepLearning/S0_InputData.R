library(TTR)
library(zoo)
library(xts)
library(quantmod)
filePath <- './EURUSD60.csv'
#--------------------	S0  导入数据并计算所需价格值
S0_FileToXTS <- function(file) {
  csvData <- read.csv(file,header = F)
  dateTimeString <- paste(as.character(csvData$V1),as.character(csvData$V2))
  dateTime <- as.POSIXlt(strptime(dateTimeString,"%Y.%m.%d %H:%M"))
  xtsData <- as.xts(read.zoo(cbind(dateTime,csvData[,3:7]),header=T))
  colnames(xtsData) <- c('Open','High','Low','Close','Volume')
  return(xtsData)
}
PRICE <- S0_FileToXTS(filePath)
Med <- (PRICE$High+PRICE$Low)*0.5
CO <- (PRICE$Open+PRICE$Close)*0.5
colnames(Med) <- 'Med'
colnames(CO) <- 'CO'
PRICE <- cbind(PRICE,Med,CO)
#chartSeries(PRICE)

#--------------------	S1  计算生成指标值
S1_FunctionValues <- function(price,p = 16) { #这里可以考虑定制MT4指标函数，再使用
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
  stoh<-stoch(price[ ,2:4],14, 3, 3)
  smi <- SMI(price[ ,2:4],n = p, nFast = 2, nSlow = 25, nSig = 9)
  colnames(smi) <- c('SMI','SMIS')
  vol <- volatility(price[ ,1:4],n = p,calc="yang.zhang", N=96)
  colnames(vol) <- 'Volatility'
  FunctionValues <- cbind(adx, ar, cci, chv, cmo, macd, osma, rsi, stoh, smi, vol)
  return(FunctionValues) 
}
FUNCTIONVALUE <- S1_FunctionValues(PRICE)

S2_ZigZagValue <- function(price,ch = 0.0037) {
  zzValue <- ZigZag(price[ ,'Med'], change = ch, percent = F, retrace = F, lastExtreme = F) #T)
  naIndex <- which(is.na(zzValue[,1]))
  if(length(naIndex)>0) zzValue[naIndex,1] <- zzValue[naIndex-1,1]
  Signal <-sign(round(diff(zzValue),8))
  Signal[which(Signal[,1]==0),1] <- NA
  Signal[which(Signal[,1]==1),1] <- 0
  Signal[which(Signal[,1]==-1),1] <- 1
  colnames(Signal) <- 'ZigZagV'
  return(Signal)
}
ZIGZAGVALUE <- S2_ZigZagValue(PRICE)
FUNCTIONVALUE <- na.omit(cbind(FUNCTIONVALUE,ZIGZAGVALUE))

library(caret)
Balancing <- function(data,col) {
  #Calculate a table with a number of classes
  LStable <- table(subset(data,select = col))
  ratioB <- max(LStable)/min(LStable)
  if(ratioB <= 1.05)  return(data)
  

  #BalancedData <- as.matrix(cbind(row.names(as.matrix(data)),as.matrix(data)[,2:18]))
  #print(row.names(as.matrix(data)))
  #print(head(BalancedData))
  #print(ncol(BalancedData))
  
  BalancedData <- upSample(x = subset(data,select = which(colnames(data)!=col)), y = as.factor(subset(data,select = col)),yname = 'Y')
  
  #print(head(BalancedData))
  
  
  #BalancedData <- upSample(x = data[ ,-ncol(DT)],y = as.factor(DT[ , ncol(DT)]), yname = "Y")

  #Convert Y (factor) into a number
  BalancedData$Y <- as.numeric(BalancedData$Y)
  #Recode Y from 1,2 into 0,1
  BalancedData$Y <- ifelse(BalancedData$Y == 1, 0, 1)
  #Convert dataframe to matrix
  BalancedData <- as.matrix(BalancedData)
  return(BalancedData)
}

BALANCEDDATA <- Balancing(FUNCTIONVALUE,'ZigZagV')

X <- subset(BALANCEDDATA,select = -Y)
Y <- subset(BALANCEDDATA,select = Y)

library(rminer)
T <- holdout(Y,ratio = 8/10, mode = "random")


spSign <- preProcess(X[T$tr, ], method = "spatialSign")
X.TRAINING <- predict(spSign, X[T$tr, ])
X.TEST     <- predict(spSign, X[T$ts, ])

# 以下为草稿

