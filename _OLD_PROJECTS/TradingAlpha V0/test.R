data("iris")
library(nnet)
set.seed(2)

ind <- sample(2, nrow(iris), replace = T, prob = c(0.7, 0.3))

trainset <- iris[ind == 1, ]
testset <- iris[ind == 2, ]

iris.nn <- nnet(Species ~ ., data = trainset, size = 2, rang = 0.1, decay = 5e-4, maxit = 200)


summary(iris.nn)


iris.predict <- predict(iris.nn, testset, type = 'class')

nn.table <- table(testset$Species, iris.predict)

iris.predict
