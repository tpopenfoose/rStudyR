library(compiler)
library(R6)

#### 简单测试 ####
N <- 1E6
naive.vector.of.squares <- function(n) {
  v <- 1:n
  for (i in 1:n) {
    v[i] <- v[i]^2
  }
}
# 函数运行
# 用户 系统 流逝 
# 1.36 0.02 1.47

compiled.naive.vector.of.squares <- cmpfun(naive.vector.of.squares)
# cmpfun运行，结果有优化
# 用户 系统 流逝 
# 0.08 0.00 0.08 

naive.vector.of.squares2 <- function(n) {
  naive.vector.of.squares(n)
}
compiled.naive.vector.of.squares2 <- cmpfun(naive.vector.of.squares2)
# cmpfun嵌套函数运行，结果没有优化
# 用户 系统 流逝 
# 1.34 0.03 1.43

compiled.naive.vector.of.squares3 <- cmpfun(function(n) {
  v <- 1:n
  for (i in 1:n) {
    v[i] <- v[i]^2
  }
})
# cmpfun运行，结果有优化
# 用户 系统 流逝 
# 0.08 0.01 0.09 

R6Compiler <- R6Class(
  public = list(
    TEST1.NORMAL = naive.vector.of.squares,
    TEST2.CMPFUN = compiled.naive.vector.of.squares,
    TEST3.CMPINFUN = compiled.naive.vector.of.squares2,
    TEST4.CMPFUN2 = compiled.naive.vector.of.squares3,
    TEST = function(n) {
      compiled.naive.vector.of.squares(n)
    }
  )
)


TEST <- R6Compiler$new()
system.time(TEST$TEST1.NORMAL(N))
# 用户 系统 流逝 
# 1.39 0.00 1.48

system.time(TEST$TEST2.CMPFUN(N))
# 用户 系统 流逝 
# 1.41 0.00 1.47 

system.time(TEST$TEST3.CMPINFUN(N))
# 用户 系统 流逝 
# 1.36 0.00 1.42 

system.time(TEST$TEST4.CMPFUN2(N))
# 用户 系统 流逝 
# 1.28 0.00 1.46 

system.time(TEST$TEST(N))
# 唯一一种可以应用到cmp的方法
# 用户 系统 流逝 
# 0.08 0.00 0.07