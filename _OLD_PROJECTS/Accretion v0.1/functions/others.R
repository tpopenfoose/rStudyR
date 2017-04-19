## xts去重复
unique.xts <- function(xts) {
  xts[!duplicated(index(xts))]
}

## 向量众数
statistic.mode <- function(vector) {
  which.max(table(vector))
}
