#### 2017-01-10 ####

naive.vector.of.squares <- function(n) {
  v <- 1:n
  for (i in 1:n)
    v[i] <- v[i]^2
}
system.time(naive.vector.of.squares(1000000))
# 用户 系统 流逝 
# 1.81 0.02 1.86 

library(compiler)
compiled.naive.vector.of.squares <- cmpfun(naive.vector.of.squares)
system.time(compiled.naive.vector.of.squares(1000000))
# 用户 系统 流逝 
# 0.13 0.00 0.12 

naive.vector.of.squares2 <- function(n) {
  naive.vector.of.squares(n)
}
compiled.naive.vector.of.squares2 <- cmpfun(naive.vector.of.squares2)
system.time(compiled.naive.vector.of.squares2(1000000))
# 用户 系统 流逝 
# 1.78 0.00 1.80 

compiled.naive.vector.of.squares3 <- cmpfun(function(n) {
  v <- 1:n
  for (i in 1:n)
    v[i] <- v[i]^2
})
system.time(compiled.naive.vector.of.squares3(1000000))
