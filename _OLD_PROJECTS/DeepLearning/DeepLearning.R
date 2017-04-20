rm(list = ls())
# Libraries
library(TTR)
library(zoo)
library(xts)
library(quantmod)
library(caret)
library(rminer)
library(deepnet)
library(doParallel)
library(foreach)
# 函数脚本文件
source('Functions.R', encoding = 'UTF-8')
# 文件路径
FILE <- './EURUSD60.csv'
# 产生所需价格相关值
PRICE <- FileToXTS(FILE)

RESULT <- DeepLearning(PRICE, h = c(30, 30, 30), LR = 0.7, Ep = 300, dec = 1, ans = 4)

plot(RESULT$BALANCE)
#lines(RESULT$BALANCE, col=2)
#print(system.time(Bal41 <- Testing.2(TABLEB.B, TABLE, h = c(30, 30, 30), LR = 0.7, Ep = 300, dec = 1, ans = 4)))
#plot(Bal41, t = "l")