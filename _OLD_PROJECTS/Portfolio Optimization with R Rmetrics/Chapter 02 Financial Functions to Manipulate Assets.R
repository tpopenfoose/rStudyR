#>>>>>>>>>> Chapter 02 Financial Functions to Manipulate Assets
library(fPortfolio)
library(fImport)

# R also has packages for downloading data from commercial sources; these include for example the RBloomberg (Sams, 2009)
#   and IBrokers (Ryan, 2008) packages.

#>>>>>  2.1 Price and Index Series
#--------------------------------------------------#
# Function:
# returns     ## generates returns from a price/index series
# cumulated   ## generates indexed values from a returns series
# drawdowns   ## computes drawdowns from financial returns
# lowess      ## smooths a price/index series
# turnpoints  ## finds turnpoints for a smoothed price/index series
#--------------------------------------------------#

#>>>>>  2.2 Return and Cumulated Return Series
#--------------------------------------------------#
# Function:
# returns    ## generates returns from a price/index series
# cumulated  ## generates indexed values from a returns series
# 
# Arguments:
# x          ## a price/index for a uni or multivariate series of class timeSeries
# methods    ## the method of computing the returns "continuous", "discrete", "compound", "simple"
# percentage ## a logical, should percentual returns be computed?
#--------------------------------------------------#
LP25 <- SWX[, 'LP25']/as.numeric(SWX[1, 'LP25'])
head(LP25, 5)

head(returns(LP25), 5)

head(cumulated(returns(LP25)), 5)

head(returns(cumulated(returns(LP25))), 4)

#>>>>>  2.3 Drawdowns Series
#--------------------------------------------------#
# Function:
# drawdowns ## computes drawdowns from financial returns
# 
# Arguments:
# x         ## a 'timeSeries' of financial returns
#--------------------------------------------------#
head(drawdowns(SWX.RET[, 1:4]))

#>>>>>  2.4 Durations Series
#--------------------------------------------------#
# Function:
# intervals ## computes intervals from a financial series
# Arguments:
# x         ## a 'timeSeries' of financial returns
#--------------------------------------------------#
SPI <- SWX[, "SPI"]
SPI10 <- SPI[c(4, 9, 51, 89, 311, 513, 756, 919, 1235, 1648), ]
SPI10

intervals(SPI10)/(24 * 3600) ## 无此函数

#>>>>>  2.5 How to Add Your Own Functions
















