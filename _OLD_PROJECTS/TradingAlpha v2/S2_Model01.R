

## classification model with recursive partitioning trees 

## 优点：
##  *.  非常容易解释
##  *.  适用于分类和回归问题
##  *.  无参数
## 缺点：
##  *.  容易出现偏差和过拟合

library(rpart)
library(caret)


## S2.1 model building
Model.Build <- function(prep.data) {
  tr <- prep.data$TR
  rp.model <- rpart(OUTPUT ~ ., data = tr)
  rp.model
}

## S2.2 model estimation
Model.Estimate <- function(model, ts.data) {
  pred <- predict(model, ts.data, type = 'class') ## 输出必须为factor格式
  confusionMatrix(pred, ts.data$OUTPUT)
}

## S2.3 model predict
Model.Predict <- function(model, newdata) {
  if(is.xts(newdata)) newdata <- as.data.frame(newdata)
  pred <- predict(model, ts.data, type = 'class')
}