
## china stocks

stock.china.code2symbol <- function(code) {
  if(is.numeric(code)) {
    code <- as.character(code)
    n.char <- nchar(code)
    if(n.char < 6) {
      code <-paste0(paste0(rep('0', 6 - n.char), collapse = ''), code, collapse = '')
    }
  }
  if(nchar(code) > 6) {
    return('ERROR')
  }
  code
}

stock.china.board <- function(symbol) {
  if(is.numeric(symbol)) {
    symbol <- stock.china.code2symbol(symbol)
  }
  symbol.numeric <- suppressWarnings(as.numeric(symbol))
  if(is.na(symbol.numeric)) {
    return('ERROR')
  }
  if(symbol.numeric == 1) {
    return('SH COMPOSITE INDEX')
  }
  if(symbol.numeric == 399001) {
    return('SZSE COMPONENT INDEX')
  }
  head2char <- substr(symbol, 1, 2)
  switch (head2char,
          '60' = 'SH A',
          '00' = switch (substr(symbol, 3, 3),
                      '0' = 'SZ A',
                      '2' = 'SZ SME'
          ),
          '30' = 'SZ SB',
          '90' = 'SH B',
          '20' = 'SZ B',
          '43' = 'NEEQ',
          'ERROR'
  )
}

stock.china.exchange <- function(symbol) {
  board <- stock.china.board(symbol)
  head2char <- substr(board, 1, 2)
  switch (head2char,
    'SH' = 'sh',
    'SZ' = 'sz',
    NULL
  )
}

stock.china.symbol.name <- function(code) {
  require(RCurl)
  symbol <- stock.china.code2symbol(code)
  exchange <- stock.china.exchange(symbol)
  url <- paste0('http://hq.sinajs.cn/list=', exchange, symbol)
  str <- getURL(url, .encoding = 'CE_UTF8')
  str.list <- strsplit(str, '"')
  str.info <- str.list[[1]][2]
  info.list <- strsplit(str.info, ',')
  info.list[[1]][1]
}

stock.china.prices <- function(code, exchange = c('sh', 'ss', 'sz'), from = '1970-01-01', to = Sys.Date()) {
  require(quantmod)
  if(missing(exchange)) {
    exchange <- stock.china.exchange(code)
    if(is.null(exchange)) {
      return(NULL)
    }
  }
  symbol <- stock.china.code2symbol(code)
  if(exchange == 'sh') {
    exchange <- 'ss'
  }
  symbol.code <- paste(symbol, exchange, sep = '.')
  prices <- suppressWarnings(getSymbols(symbol.code, auto.assign=FALSE, from = from, to = to))
  colnames(prices) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
  prices
}

## china futures

future.china.symbol.informations.builder <- function() {
  require(RCurl)
  exchanges <- c(czce = '郑商所', dce = '大商所', shfe = '上期所', cffex = '中金所')
  czce <- c(CF = '棉花', FG = '玻璃', JR = '粳稻', LR = '晚籼稻', MA = '郑醇',
            OI = '菜油', RI = '早籼稻', RM = '菜粕', RS = '菜籽', SF = '硅铁',
            SM = '锰硅', SR = '白糖', TA = 'PTA', WH = '强麦', ZC = '动力煤')
  dce <- c(A = '豆一', B = '豆二', BB = '胶合板', C = '玉米', CS = '玉米淀粉',
           FB = '纤维板', I = '铁矿石', J = '焦炭', JD = '鸡蛋', JM = '焦煤',
           L = '塑料', M = '豆粕', P = '棕榈', PP = 'PP', V = 'PVC', Y = '豆油')
  shfe <- c(AU = '黄金', BU = '沥青', FU = '燃油', HC = '热轧卷板', RB = '螺纹钢',
            RU = '橡胶', WR = '线材', AG = '白银', AL = '沪铝', CU = '沪铜',
            NI = '沪镍', PB = '沪铅', SN = '沪锡', ZN = '沪锌')
  cffex <- c(IC = '中证500指数期货', IF = '沪深300指数期货', IH = '上证50指数期货', T = '10年期国债期货', TF = '5年期国债期货')
  all.symbols <- c(czce, dce, shfe, cffex)
  all.exchanges <- c(rep(exchanges['czce'], length(czce)),
                     rep(exchanges['dce'], length(dce)),
                     rep(exchanges['shfe'], length(shfe)),
                     rep(exchanges['cffex'], length(cffex)))
  data.frame(
    stringsAsFactors = F,
    row.names = NULL,
    Exchange.Name = all.exchanges,
    Exchange.Code = names(all.exchanges),
    Symbol.Name = all.symbols,
    Symbol.Code = names(all.symbols)
  )
}

future.china.exchange.code <- function(code, symbol.table = future.china.symbol.informations.builder()) {
  symbol.table$Exchange.Code[which(symbol.table$Symbol.Code == code)]
}

future.china.symbol.name <- function(code, symbol.table = future.china.symbol.informations.builder()) {
  symbol.table$Symbol.Name[which(symbol.table$Symbol.Code == code)]
}

future.china.prices <- function(code, contract = '0', from = '1970-01-01', to = Sys.Date()) {
  require(XML)
  require(htmlTable)
  require(quantmod)
  if(is.numeric(contract)) {
    contract <- as.character(contract)
  }
  pre.url.builder <- function(code, contract, from, to) {
    exchange <- future.china.exchange.code(code)
    paste0('http://vip.stock.finance.sina.com.cn/q/view/vFutures_History.php?&jys=', exchange,
           '&pz=', code, '&hy=', code, contract, '&breed=', code, contract, '&type=cffex&start=', from, '&end=', to, '&page=')
  }
  url.temp <- pre.url.builder(code, contract, from, to)
  tables <- readHTMLTable(paste0(url.temp, '1'), as.data.frame = T, stringsAsFactors = FALSE)
  price <- tables[[4]]
  tables.row <- nrow(price)
  if(tables.row <= 2) {
    return(NULL)
  }
  price <- price[3:tables.row, ]
  if(!is.null(tables[[5]])) {
    page.temp <- colnames(tables[[5]])
    page.temp <- gsub('[^[:digit:]]', '', page.temp)
    page.temp <- substr(page.temp, 2, nchar(page.temp))
    pages <- as.numeric(page.temp)
    if(pages > 1) {
      price.table <- function(page.number = 1, pre.url = url.temp) {
        whole.url <- paste0(pre.url, as.character(page.number))
        price.data.frame <- readHTMLTable(whole.url, as.data.frame = T, stringsAsFactors = FALSE)[[4]]
        price.data.frame[3:nrow(price.data.frame), ]
      }
      pages.vector <- 2:pages
      price.on.pages <- lapply(pages.vector, FUN = price.table, pre.url = url.temp)
      price.on.pages <- do.call(rbind, c(price.on.pages, make.row.names = F))
      price <- rbind(price, price.on.pages, make.row.names = F)
    }
  }
  with(price, {
    xts(data.frame(
      Open = as.numeric(V2),
      High = as.numeric(V3),
      Low = as.numeric(V4),
      Close = as.numeric(V5),
      Volume = as.numeric(V6)
    ),
    as.POSIXct(V1))
  })
}

