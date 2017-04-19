# SMA @library(TTR)
IND_SMA <- function (x, n = 10, ...) {
  sma <- MovMean(x, n) # ?MovMean?
  if (!is.null(dim(sma))) {
    colnames(sma) <- "SMA"
  }
  return(sma)
}
# MovMean 移动平均 @library(TTR)
MovMean <- function (x, n = 10, cumulative = FALSE) {
  if (cumulative) {
    result <- MovSum(x, n, cumulative)/1:NROW(x) # ?MovSum?
  } else {
    result <- MovSum(x, n)/n
  }
  return(result)
}
# MovSum 移动求和 @library(TTR)
MovSum <- function (x, n = 10, cumulative = FALSE) {
  x <- try.xts(x, error = as.matrix) # ?try.xts?
  if (n < 1 || n > NROW(x)) stop("Invalid 'n'")
  NAs <- sum(is.na(x))
  if (NAs > 0) {
    if (any(is.na(x[-(1:NAs)]))) stop("Series contains non-leading NAs")
    if (NAs + n > NROW(x)) stop("not enough non-NA values")
  }
  beg <- 1 + NAs
  len <- NROW(x) - NAs
  result <- double(NROW(x))
  if (cumulative) {
    result[beg:NROW(x)] <- cumsum(x[beg:NROW(x)])
  }
  else {
    result[(n + beg - 1)] <- sum(x[beg:(n + beg - 1)])
    result <- .Fortran("runsum", ia = as.double(x[beg:NROW(x)]), 
                       lia = as.integer(len), n = as.integer(n), oa = as.double(result[beg:NROW(x)]), 
                       loa = as.integer(len), PACKAGE = "TTR", DUP = TRUE)$oa
    result <- c(rep(NA, NAs), result)
  }
  is.na(result) <- c(1:(n - 1 + NAs))
  reclass(result, x)
}
# try.xts @library(xts)
try.xts <- function (x, ..., error = TRUE) {
  if (is.xts(x)) {
    return(x)
  }
  xx <- try(as.xts(x, ..., .RECLASS = TRUE), silent = TRUE) # ?try?
  if (inherits(xx, "try-error")) {
    if (is.character(error)) {
      stop(error) # ?stop?
    } else if (is.function(error)) {
      return(error(x, ...))
    } else if (error) {
      stop(gsub("\n", "", xx))
    } else {
      return(x)
    }
  } else {
    structure(xx, .RECLASS = TRUE)
  }
}
# is.xts @library(xts)
is.xts <- function (x)  {
  inherits(x, "xts") && is.numeric(.index(x)) && !is.null(indexClass(x)) # ?inherits?.index?indexClass?
}