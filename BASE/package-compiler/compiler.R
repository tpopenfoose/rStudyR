library(compiler)


#### 简单测试 ####
N <- 1E6
naive.vector.of.squares <- function(n) {
  v <- 1:n
  for (i in 1:n) {
    v[i] <- v[i]^2
  }
}
system.time(naive.vector.of.squares(N))
# 函数运行
# 用户 系统 流逝 
# 1.36 0.02 1.47

compiled.naive.vector.of.squares <- cmpfun(naive.vector.of.squares)
system.time(compiled.naive.vector.of.squares(N))
# cmpfun运行，结果有优化
# 用户 系统 流逝 
# 0.08 0.00 0.08 

naive.vector.of.squares2 <- function(n) {
  naive.vector.of.squares(n)
}
compiled.naive.vector.of.squares2 <- cmpfun(naive.vector.of.squares2)
system.time(compiled.naive.vector.of.squares2(N))
# cmpfun嵌套函数运行，结果没有优化
# 用户 系统 流逝 
# 1.34 0.03 1.43

compiled.naive.vector.of.squares3 <- cmpfun(function(n) {
  v <- 1:n
  for (i in 1:n) {
    v[i] <- v[i]^2
  }
})
system.time(compiled.naive.vector.of.squares3(N))
# cmpfun运行，结果有优化
# 用户 系统 流逝 
# 0.08 0.01 0.09 