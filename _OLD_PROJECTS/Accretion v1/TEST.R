rm(list = ls())
require(R6)
require(quantmod)
source(file = './class/instrument.R', encoding = 'UTF-8')
source(file = './class/exchange.R', encoding = 'UTF-8')
source(file = './class/symbol.R', encoding = 'UTF-8')


### TEST001 {
# test_001 <- R6C_Instrument_Forex$new('IC Markets')
# test_001$getType()
# test_001$getExchange()$getName()
# test_001$getExchange()$getRule()
# test_001$getExchange()$getTimeZone()
# test_001$getExchange()$getRegion()
### TEST001 }

test_002 <- R6C_Instrument_Forex$new()
test_002$getType()
test_002$add.exchange('ICMarkets')
test_002$getExchanges()
test_002$add.symbol('ICMarkets', 'EURUSD')
test_002$getExchanges()[['ICMarkets']]$getSymbols()
