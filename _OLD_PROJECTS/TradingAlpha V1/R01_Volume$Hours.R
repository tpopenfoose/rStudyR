
library(quantmod)
findPeaks(t.EURUSD$Close)
findValleys(t.EURUSD$Close)

cor.test(x = t.EURUSD$Volume, y = abs(t.EURUSD$Close - t.EURUSD$Open)) ## 0.668 相关度

## 如何按照一定时间段分组，比如小时

# library(rportfolios)
# library(timeSeries)
# ts.EURUSD <- as.timeSeries.xts(t.EURUSD)
# # applySeries(ts.EURUSD, by = 'Hour')
# 
# index(ts.EURUSD)
# aa <- time(ts.EURUSD)[1]


aa <- index(t.EURUSD)
aa <- as.POSIXlt(aa)
aa$hour == 7

GetXTSHour <- function(xts) { ## 按小时分组
  times <- as.POSIXlt(index(xts))
  days <- times$hour
}