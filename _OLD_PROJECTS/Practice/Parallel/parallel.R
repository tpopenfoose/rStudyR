# package:  parallel
library(parallel) 	        # 加载包
cl.cores <- detectCores() 	# 检查当前电脑可用核数
cl <- makeCluster(cl.cores)	# 使用刚才检测的核并行运算
stopCluster(cl)             # 终止并行运算

# package:  doParallel,foreach
library(foreach)