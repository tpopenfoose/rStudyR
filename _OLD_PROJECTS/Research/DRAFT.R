# 
# 
# MT4csvToXTS <- function(file) {
#   symbol <- read.csv(file, header = F)
#   time.str <- paste(symbol$V1, symbol$V2)
#   xts.data <- xts(symbol[, 3:7], as.POSIXct(time.str, format = '%Y.%m.%d %H:%M'))
#   colnames(xts.data) = c('Open', 'High', 'Low', 'Close', 'Volume')
#   xts.data
# }
# 
# 
# 
# t.EURUSD <- MT4csvToXTS('EURUSD60.csv')
# 
# print(t.EURUSD)

aaa <- MT4csvToXTS('EURUSD60-2.csv')
bbb <- aaa['2016-03-20/']
dim(bbb)
View(bbb)


input <- Data.Input(bbb)
output <- GetSignals(model.building, input)

result <- cbind(bbb, Output = output)
result <- na.omit(result)

result2 <- cbind(result, Output.mean = mean.out)
result2 <- na.omit(result2)


sign <- result2$Output - result2$Output.mean
sign <- sign(sign)


