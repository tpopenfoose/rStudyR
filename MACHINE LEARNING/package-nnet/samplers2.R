Var1 <- c(rnorm(50, 1, 0.5), rnorm(50, -0.6, 0.2)) %T>% print
Var2 <- c(rnorm(50, -0.8, 0.2), rnorm(50, 2, 1)) %T>% print
x <- matrix(c(Var1, Var2), nrow = 100, ncol = 2) %T>% print
y <- c(rep(1, 50), rep(0, 50)) %T>% print
nn <- nnet(x, y,size = 2, rang = 0.1,
           decay = 5e-4, maxit = 200)

test_Var1 <- c(rnorm(50, 1, 0.5), rnorm(50, -0.6, 0.2)) %T>% print
test_Var2 <- c(rnorm(50, -0.8, 0.2), rnorm(50, 2, 1)) %T>% print
test_x <- matrix(c(test_Var1, test_Var2), nrow = 100, ncol = 2) %T>% print

yy <- predict(nn, test_x, type = 'raw')#%T>% plot
