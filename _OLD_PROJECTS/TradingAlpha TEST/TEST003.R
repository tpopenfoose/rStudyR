## TEST003  A股价格获取 ; 爬虫

## 结果：可以正常获取 1.股票中文名称，日线图价格
## 注意：股市策略注意不可做多，不要以外汇方式进行测试

library(quantmod)


# setSymbolLookup(WK=list(name='601006.ss',src='yahoo'))
# getSymbols("WK")
# chartSeries(WK)
# 
# 
# aaa <- getSymbols('000897.sz',auto.assign=FALSE)
# 
# 
# library(RCurl)
# bbb <- getURL('http://hq.sinajs.cn/list=sh601006', .encoding = 'CE_UTF8')

library(RCurl)
AStock <- function(code, exchange = c('sh', 'sz')) {
  code <- as.character(code)
  getName.CN <- function(code, exchange) {
    url <- paste0('http://hq.sinajs.cn/list=', exchange, code)
    str <- getURL(url, .encoding = 'CE_UTF8')
    str.list <- strsplit(str, '"')
    str.info <- str.list[[1]][2]
    info.list <- strsplit(str.info, ',')
    info.list[[1]][1]
  }
  name <- getName.CN(code, exchange)
print(name)
  if(exchange == 'sh') name.str <- '.ss'
  else name.str <- '.sz'
  name.str <- paste0(code, name.str)
print(name.str)
  # setSymbolLookup(WK = list(name = name.str, src = 'yahoo'))
  # getSymbols('WK')
  getSymbols(name.str, auto.assign=FALSE)
}