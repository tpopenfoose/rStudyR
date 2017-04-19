# setup <- RSQLite::dbConnect(RSQLite::SQLite(), dbname="test.db")
# TSsql::removeTSdbTables(setup, yesIknowWhatIamDoing=TRUE)
# TSsql::createTSdbTables(setup, index=FALSE)
# DBI::dbListTables(setup)
# # DBI::dbDisconnect(setup)
# 
# require(TSdbi)
# library("TSSQLite")
# con <- TSconnect("SQLite", dbname="test.db")


# require(TSSQLite)
# 
# con <- TSconnect("SQLite", dbname='forex.price')
# 
# TSsql::createTSdbTables(forex.price)
# 
# 
# TSdates()
# TSconnect(forex.price, 'A')

require(RSQLite)

forex.price <- dbConnect(RSQLite::SQLite(), "test")
forex.price
dbListTables(forex.price)
dbGetInfo(forex.price)
dbListFields(forex.price, 'price')
# 
dbRemoveTable(forex.price, 'price')


dbSendQuery(forex.price, 'create table price(time timestamp, open double precision, high double precision, low double precision, close double precision, volume double precision);')
# sqlCreateTable(forex.price, 'EEE', fields = c(a = "integer", b = "text"), temporary = T)
require(quantmod)

abc <- data.frame(
  row.names = NULL,
  # id = NA,
  time = as.character(index(EURUSD.xts)),
  close = EURUSD.xts$Close,
  open = EURUSD.xts$Open,
  high = EURUSD.xts$High,
  low = EURUSD.xts$Low,
  volume = EURUSD.xts$Volume
)
## 表格不识别colname,不对其进行对齐

dbWriteTable(forex.price, 'price', abc, append = T)



dbReadTable(forex.price, 'price') -> eee


dbGetQuery(forex.price, "SELECT * FROM price WHERE time > '2015-09-22 08:00:00'") -> fff


dbDisconnect(forex.price)

# require(RODBC)
# forex.price <- odbcConnect('ForexPrice')
# 
# odbcGetInfo(forex.price)
# 
# sqlTables(forex.price)
# 
# # sqlFetch(forex.price, 'create database tablename')
# 
# sqlQuery(forex.price, 'create table tablename(column1 char, column2 char, column3 char);')
# 
# abc <- data.frame(
#   c(1,2,3),
#   c('A', 'B', 'C')
# )
# #
# # sqlSave(forex.price, dat = abc, tablename = 'TEST')
# odbcClose(forex.price)

