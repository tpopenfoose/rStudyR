#>>>>>>>>>> Chapter 01 Generic Functions to Manipulate Assets
library(fPortfolio)
#>>>>>  1.1 timeDate and timeSeries Objects
#--------------------------------------------------#
# Data Sets:
# SWX        ## Daily Swiss equities, bonds, and reits series
# LPP2005    ## Daily Pictet Swiss pension fund benchmarks
# SPISECTOR  ## Swiss Performance sector indexes
# GCCINDEX   ## Gulf Cooperation Council equity indexes
# SMALLCAP   ## Monthly selected US small capitalized equities
# MSFT       ## Daily Microsoft open, high, low, close, volume
#--------------------------------------------------#

#--------------------------------------------------#
# Function:
# timeDate      ## creates timeDate objects from scratch
# timeSeq, seq  ## creates regularly spaced timeDate objects
# timeCalendar  ## creates timeDate objects from calendar atoms
# as.timeDate   ## coerces and transforms timeDate objects
# timeSeries    ## creates a timeSeries object from scratch
# readSeries    ## reads a timeSeries from a spreadsheet file.
# as.timeSeries ## coerces and transforms timeSeries objects
#--------------------------------------------------#

#>>>>>  1.2 Loading timeSeries Data Sets
class(SWX.RET)
colnames(SWX.RET)
head(SWX.RET[, 1:3])

# create small data set
data <- head(SWX.RET[, 1:3])
# write data to a CSV file in the current directory
write.csv(data, file = "myData.csv")

# write CSV file in current directory, specifying the separators
data2 <- readSeries(file = "myData.csv", header = TRUE, sep = ",") ## readSeries from package@timeSeries

args(readSeries)

# The Rmetrics **fImport** (Würtz, 2009c) provides several functions to download time series data from the Internet, for example from
library(fImport)
#--------------------------------------------------#
# Download Functions:
# fredSeries  ## imports market data from the US Federal Reserve
# oandaSeries ## imports FX market data from OANDA
# yahooSeries ## imports market data from Yahoo Finance
#--------------------------------------------------#

#>>>>>  1.3 Sorting and Reverting Assets
head(SWX)
end(SWX)
class(SWX)
#--------------------------------------------------#
# Functions:
# sort   ## sorts a 'timeSeries' in ascending or descending order
# rev    ## provides a time-reversed version of a 'timeSeries'
# sample ## generates a sample either with or without replacement
#
# Arguments:
# x      ## an object of class 'timeSeries'
#--------------------------------------------------#

SAMPLE <- sample(SWX[1:10, ])
SAMPLE

sort(SAMPLE)
rev(sort(SAMPLE))
sort(SAMPLE, decreasing = TRUE)

#>>>>>  1.4 Alignment of Assets
nrow(SWX)
ALIGNED <- align(x = SWX, by = '1d', method = "before", include.weekends = FALSE) ## also filter for periodic date
nrow(SWX)
#--------------------------------------------------#
# Function:
# align   ## aligns a 'timeSeries' object to calendar objects.
# Arguments:
# x       ## an object of class 'timeSeries' 
# by      ## a character string formed from an integer length and a period identifier.
#            Valid values are "w", "d", "h","m", "s", for weeks, days, hours, minutes
#            and seconds. For example, a bi-weekly period is expressed as "2w" 
# offset  ## a character string formed from an integer length and a period identifier in the same way as for 'by'
# method  ## a character string, defining the alignment. Substitutes a missing record with the value of the previous
#            ("before") record, of the following ("after") record, interpolates ("interp") or fills with NAs ("NA")
# include.weekends ## should the weekend days (Saturdays and Sundays) be included?
#--------------------------------------------------#

#>>>>>  1.5 Binding and Merging Assets
#--------------------------------------------------#
# Function:
# c       ## concatenates a 'timeSeries' object.
# cbind   ## combines a 'timeSeries' by columns.
# rbind   ## combines a 'timeSeries' by rows.
# merge   ## merges two 'timeSeries' by common columns and/or rows.
# Arguments:
# x, y    ## objects of class 'timeSeries'.
#--------------------------------------------------#
set.seed(1953)
charvec <- format(timeCalendar(2008, sample(12, 6)))
data <- matrix(round(rnorm(6), 3))
t1 <- sort(timeSeries(data, charvec, units = "A"))
t1

charvec <- format(timeCalendar(2008, sample(12, 9)))
data <- matrix(round(rnorm(9), 3))
t2 <- sort(timeSeries(data, charvec, units = "B"))
t2

charvec <- format(timeCalendar(2008, sample(12, 5)))
data <- matrix(round(rnorm(10), 3), ncol = 2)
t3 <- sort(timeSeries(data, charvec, units = c("A", "C")))
t3

cbind(t1, t2)

cbind(t1, t3) ## 如果列名重复，自动重命名 X.1, X.2 ...

rbind(t1, t2) ## 时间可能重复

tM <- merge(merge(t1, t2), t3)
tM ## 如果列名重复，合并在一列

#>>>>>  1.6 Subsetting Assets
#--------------------------------------------------#
# Function:
# [       ## extracts or replaces subsets by indexes, column names, date/time stamps, logical predicates, etc
# subset  ## returns subsets that meet specified conditions
# window  ## extracts a piece between two 'timeDate' objects
# start   ## extracts the first record
# end     ## extracts the last record
# 
# Arguments:
# x       ## an object of class 'timeSeries'
#--------------------------------------------------#
SWX[2:5, ]

SWX[2:5, 2]

SWX[start(SWX), ]

SWX[start(sample(SWX)), ]

tail(SWX[, "SPI"])

SWX["2007-04-24", ]

round(window(SWX, start = "2006-01-15", end = "2006-01-21"), 1)

#>>>>>  1.7 Aggregating Assets
#--------------------------------------------------#
# Function:
# aggregate  ## aggregates a 'timeSeries' object.
# Arguments:
# x          ## is a uni- or multivariate 'timeSeries' object
# by         ## is a 'timeDate' sequence of grouping dates
# FUN        ## a scalar function to compute the summary statistics to be applied to all data subsets
#--------------------------------------------------#
charvec <- timeCalendar()
data <- matrix(round(runif(24, 0, 10)), 12)
tS <- timeSeries(data, charvec)
tS

by <- unique(timeLastDayInQuarter(charvec))
by

aggregate(tS, by, FUN = sum, units = c("TSQ.1", "TSQ.2"))

#--------------------------------------------------#
# Function:
# timeLastDayInMonth    ## last day in a given month/year
# timeFirstDayInMonth   ## first day in a given month/ year
# timeLastDayInQuarter  ## last day in a given quarter/year
# timeFirstDayInQuarter ## first day in a given quarter/year
# timeNdayOnOrAfter     ## date month that is a n-day ON OR AFTER
# timeNdayOnOrBefore    ## date in month that is a n-day ON OR BEFORE
# timeNthNdayInMonth    ## n-th occurrence of a n-day in year/month
# timeLastNdayInMonth   ## last n-day in year/month
#--------------------------------------------------#
tS <- 100 * LPP2005.RET[, "SPI"]
by <- timeLastDayInMonth(time(tS))
aggregate(tS, by, sum)

#>>>>>  1.8 Rolling Assets
rollapply <- function(x, by, FUN, ...) {
  ans <- applySeries(x, from = by$from, to = by$to, by = NULL, FUN = FUN, format = x@format,
                     zone = finCenter(x), FinCenter = finCenter(x),
                     title = x@title, documentation = x@documentation, ...)
  attr(ans, "by") <- data.frame(from = format(by$from), to = format(by$to)
  )
  ans
}
#--------------------------------------------------#
# Function:
# periods   ## constructs equidistantly sized and shifted windows
# 
# Arguments:
# period    ## size (length) of the periods 长度
# by        ## shift (interval) of the periods, "m" monthly, "w" weekly, "d" daily, "H" by hours, "M" by minutes,
#              "S" by seconds. 间隔
#--------------------------------------------------#
DATA <- 100 * SWX.RET[, c(1:2, 4:5)]
by <- periods(time(DATA), "12m", "1m")
SWX.ROLL <- rollapply(DATA, by, FUN = "colSums")
SWX.ROLL

by <- periods(time(SWX), period = "52w", by = "4w")
by <- periods(time(SWX), period = "360d", by = "30d")


