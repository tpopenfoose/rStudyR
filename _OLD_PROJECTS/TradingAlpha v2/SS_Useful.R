
####  about finance 
##  @ yield in point
Profit.Pips <- function(xts) {
  profit.pips <- diff(xts$Close)
  colnames(profit.pips) <- 'Profit.Pips'
  profit.pips
}
# print(head(Profit.Pips(EURUSD.xts)))

##  @ simple return
Return.Simple <- function(xts) {
  close <- xts$Close
  return.simple <- diff(close) / lag(close, -1) * 100
  colnames(return.simple) <- 'return.simple'
  return.simple
}
# print(head(Return.Simple(EURUSD.xts)))


##  @ continuously return
Return.Continuously <- function(xts) {
  close <- xts$Close
  return.continuously <- diff(log(close)) * 100
  colnames(return.continuously) <- 'return.Continuously'
  return.continuously
}
# print(head(Return.Continuously(EURUSD.xts)))

##  @ peak / valley index
Peaks.Index <- function(xts) findPeaks(xts$Close)
Valley.Index <- function(xts) findValleys(xts$Close)

####  about time
TimeFactors <- function(xts, fac = c('sec', 'min', 'hour', 'mday', 'mon', 'year', 'wday', 'yday', 'isdst', 'tz', 'tz.diff')) {
  time.factors <- c('sec', 'min', 'hour', 'mday', 'mon', 'year', 'wday', 'yday', 'isdst', 'tz', 'tz.diff')
  index <- which(fac == time.factors)
  if(length(index) == 0) return(NULL)
  times <- index(xts)
  times.xlt <- as.POSIXlt(times)
  facs <- xts(times.xlt[[index]], times)
  colnames(facs) <- paste0('Time.', toupper(fac))
  if(index == 5) facs <- facs + 1
  else if(index == 6) facs <- facs + 1900
  facs
}
