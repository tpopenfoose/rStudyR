# 欧拉第14问题
func <- function(x) {
  n = 1
  raw <- x
  while (x > 1) {
    x <- ifelse(x%%2==0,x/2,3*x+1)
    n = n + 1
  }
  return(c(raw,n))
}

x <- 1:10000

time1 <- system.time({
  results <- lapply(x,func)
  res.df <- do.call('rbind',results)
})
print(res.df[which.max(res.df[,2]),1])
print(time1)

library(parallel)
time2 <- system.time({
  cores <- detectCores()
  cl <- makeCluster(cores)  # 初始化四核心集群
  results <- parLapply(cl,x,func) # lapply的并行版本
  res.df <- do.call('rbind',results) # 整合结果
  stopCluster(cl) # 关闭集群
})
print(time2)
print(res.df[which.max(res.df[,2]),1])

# 非并行计算方式，类似于sapply函数的功能
library(foreach)
time3 <- system.time(x <- foreach(x,.combine='rbind') %do% func(x))
print(time3)

# 启用doParallel作为foreach并行计算的后端
library(foreach)
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)
time4 <- system.time(x <- foreach(x,.combine='rbind') %dopar% func(x)) # 并行计算方式
stopCluster(cl)
print(time4)

# 2016-01-29    以上测试可知：处理简单问题时，对简单计算没有明显速度提升，甚至更慢了；