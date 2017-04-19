rm(list = ls())

library(C50)
data(churn)


churnTrain <- churnTrain[,! names(churnTrain) %in% c("state", "area_code", "account_length") ]
set.seed(2)
ind <- sample(2, nrow(churnTrain), replace = TRUE, prob = c(0.7, 0.3))

trainset <- churnTrain[ind == 1,]
testset <- churnTrain[ind == 2,]
dim(trainset)
dim(testset)

library(rpart)

churn.rp <- rpart(churn ~ ., data = trainset)

churn.rp

printcp(churn.rp)

plotcp(churn.rp)

summary(churn.rp)


plot(churn.rp, margin = 0.1)
text(churn.rp, all = TRUE, use.n = TRUE)

plot(churn.rp, uniform = TRUE, branch = 0.6, margin = 0.1)
text(churn.rp, all = TRUE, use.n = TRUE)


predictions <- predict(churn.rp, testset, type = 'class')
table(testset$churn, predictions)

library(caret)
confusionMatrix(table(predictions, testset$churn))
