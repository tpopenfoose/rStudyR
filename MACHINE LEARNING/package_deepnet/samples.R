library(deepnet)
library(magrittr)

Var1 <- c(rnorm(50, 1, 0.5), rnorm(50, -0.6, 0.2)) %T>% print
Var2 <- c(rnorm(50, -0.8, 0.2), rnorm(50, 2, 1)) %T>% print
x <- matrix(c(Var1, Var2), nrow = 100, ncol = 2) %T>% print
# y <- c(rep(1, 50), rep(0, 50)) %>% as.factor %>% as.integer %>% subtract(1) %T>% print
# y <- c(rep(1, 50), rep(0, 50)) %>% classvec2classmat %T>% print
# y <- c(rep(1, 50), rep(0, 50)) %>% as.factor %>% as.integer %>% subtract(1) %T>% print
y <- c(rep(1, 50), rep(0, 50)) %T>% print
dnn <- sae.dnn.train(x, y, hidden = c(5)) %T>% print
# nn <- nn.train(x, y, hidden = c(5))
# dbn <- dbn.dnn.train(x, y, hidden = c(5))

## predict by dnn
test_Var1 <- c(rnorm(50, 1, 0.5), rnorm(50, -0.6, 0.2)) %T>% print
test_Var2 <- c(rnorm(50, -0.8, 0.2), rnorm(50, 2, 1)) %T>% print
test_x <- matrix(c(test_Var1, test_Var2), nrow = 100, ncol = 2) %T>% print
# nn.test(dnn, test_x, y) %T>% print
# nn.test(nn, test_x, y) %T>% print

yy <- nn.predict(dnn, test_x) %T>% plot
# yy2 <- nn.predict(nn, test_x) %T>% plot
# yy3 <- nn.predict(dbn, test_x) %T>% plot

# 
# ####
# Var1 <- c(rnorm(50, 1, 0.5), rnorm(50, -0.6, 0.2))
# Var2 <- c(rnorm(50, -0.8, 0.2), rnorm(50, 2, 1))
# x <- matrix(c(Var1, Var2), nrow = 100, ncol = 2)
# y <- c(rep(1, 50), rep(0, 50))
# nn <- nn.train(x, y, hidden = c(5))
# ## predict by nn
# test_Var1 <- c(rnorm(50, 1, 0.5), rnorm(50, -0.6, 0.2))
# test_Var2 <- c(rnorm(50, -0.8, 0.2), rnorm(50, 2, 1))
# test_x <- matrix(c(test_Var1, test_Var2), nrow = 100, ncol = 2)
# yy <- nn.predict(nn, test_x)
# 
# #### draft
# classvec2classmat <- function (yvec) 
# {
#   yvec <- factor(yvec)
#   nclasses <- nlevels(yvec)
#   outmat <- matrix(0, length(yvec), nclasses)
#   dimnames(outmat) <- list(NULL, levels(yvec))
#   for (i in 1:nclasses) outmat[which(as.integer(yvec) == i), i] <- 1
#   outmat
# }
# 
# classmat2classvec <- function(ymat, threshold=0.5)
# {
#   class.names <- dimnames(ymat)[[2]]
#   if (is.null(class.names)) 
#     class.names <- 1:ncol(ymat)
#   classes <- apply(ymat, 1, function(x) which(x == max(x))[1])
#   classes[apply(ymat, 1, max) < threshold] <- NA
#   class.names[classes]
# }