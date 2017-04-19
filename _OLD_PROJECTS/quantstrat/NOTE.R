## link: http://r-forge.r-project.org/R/?group_id=316
## package group:
## 1. FinancialInstrument: Financial Instrument Model Infrastructure for R
##  install.packages("FinancialInstrument", repos="http://R-Forge.R-project.org")
## 2. RTAQ: Tools for the analysis of trades and quotes in R
##  install.packages("RTAQ", repos="http://R-Forge.R-project.org")
## 3. blotter: Tools for Transaction-Oriented Trading Systems Development
##  install.packages("blotter", repos="http://R-Forge.R-project.org")
## 4. quantstrat: Quantitative Strategy Model Framework
##  install.packages("quantstrat", repos="http://R-Forge.R-project.org")

# bbandParameters                                     example of parameter test on bbands demo strategy
# bbands                                              Build a simple Bollinger Bands strategy using one indicator, three signals, and three trade rules
# bee                                                 Milktrader's Bumblebee FastMA/BBands cross system
# faber                                               demonstrate a simple long term trend model using a 10-month SMA on a small portfolio of ETFs based on
# Mebane Faber paper
# faberMC                                             apply the Faber 10-month SMA strategy to a three-currency index portfolio
# faber_rebal                                         apply the Faber 10-month SMA strategy, rebalance quarterly
# luxor.1.strategy.basic                              Jaekle & Tomasini; Sections 3.2: basic luxor strategy not using ordersets or orderchains
# luxor.2.add.paramsets                               Jaekle & Tomasini; Sections 3.3: variation of the input parameters
# luxor.3.paramset.sma                                Jaekle & Tomasini; Sections 3.3: variation of the input parameters
# luxor.4.paramset.timespan                           Jaekle & Tomasini; Sections 3.4: inserting an intraday time filter
# luxor.5.strategy.ordersets                          Jaekle & Tomasini; Sections 3.5: strategy implementation using ordersets and orderchains
# luxor.6.paramset.stoploss                           Jaekle & Tomasini; Sections 3.5: paramset implementation for stoploss optimization
# luxor.6.paramset.stoptrailing                       Jaekle & Tomasini; Sections 3.5: paramset implementation for stop trailing optimization
# luxor.6.paramset.takeprofit                         Jaekle & Tomasini; Sections 3.5: paramset implementation for take profit optimization
# luxor.7.exit.and.risk                               Jaekle & Tomasini; Sections 3.5: running with stoploss and/or stoptrailing and/or takeprofit
# luxor.8.walk.forward                                Jaekle & Tomasini; Sections 6: walk forward analysis
# luxor.getSymbols                                    Jaekle & Tomasini; reading symbols
# luxor.include                                       Jaekle & Tomasini; Sections constants
# luxor.sample.MAE.stoploss                           Jaekle & Tomasini; sample MAE stoploss graph
# luxor.sample.MAE.stoptrailing                       Jaekle & Tomasini; sample MAE stoptrailing graph
# luxor.sample.MFE.takeprofit                         Jaekle & Tomasini; sample MFE take profit graph
# luxor.sample.tradeGraphs.sma                        Jaekle & Tomasini; sample 3D SMA graph
# luxor.sample.tradeGraphs.timespan                   Jaekle & Tomasini; sample 3D timespan graph
# luxor.sample.walk.forward                           Jaekle & Tomasini; walk forward chart sample
# maCross                                             classic 'Golden Cross' 50/200 period moving average cross strategy, long only, with long/short extra
# rules commented out
# macd                                                example of Moving Average Convergence/Divergence (MACD) used as a trend indicator
# macdParameters                                      example of parameter test on macd demo strategy
# macdRebalancing                                     example of applyStrategy.rebalancing on macd demo strategy
# pair_trade                                          simple two-instrument long-short equity strategy demonstrating custom indicator and extended order
# sizing functionality
# rocema                                              Rate-of-Change EMA strategy: demonstrating ternary indicator, ordersets to handle simultaneous stop-loss
# and take-profit orders
# rsi                                                 Relative Strength Index (RSI) strategy demonstrating long-only threshold actions
# signal.RSI                                          RSI Cross Strategy with Signal Analysis Example; See rsi.R
# signal.SMA                                          SMA Cross Strategy with Signal Analysis Example; See maCross.R


demos <- 'D:/HawkSoftware/R-3.2.5/library/quantstrat/demo'
scripts <- dir(demos)

demo(package = 'quantstrat')