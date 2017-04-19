#### original
## display the names of or delete instruments, stocks, options, futures, currencies, bonds, funds, spreads, guaranteed_spreads, synthetics, derivatives, or non-derivatives.
##
{
  function (pattern = NULL, match = TRUE, verbose = TRUE) 
  {
    if (length(pattern) > 1 && !match) {
      if (verbose) 
        warning("Using match=TRUE because length of pattern > 1.")
      match <- TRUE
    }
    if (!is.null(pattern) && match) {
      symbols <- ls(.instrument, all.names = TRUE)
      symbols <- symbols[match(pattern, symbols)]
    }
    else if (!match && length(pattern) == 1) {
      symbols <- ls(.instrument, all.names = TRUE, pattern = pattern)
    }
    else if (is.null(pattern)) {
      symbols <- ls(.instrument, all.names = TRUE)
    }
    is.iname <- is.instrument.name(symbols)
    if (!any(is.iname)) 
      return(NULL)
    symbols[is.iname]
  }
}

## analysis
ls_instruments <- function (pattern = NULL, match = TRUE, verbose = TRUE) {
  if (length(pattern) > 1 && !match) { ## 如果pattern长度大于1，并且match为F
    if (verbose) ## 如果verbose为T
      warning("Using match=TRUE because length of pattern > 1.") ## 警告
    match <- TRUE ## match赋值为T
  }
  if (!is.null(pattern) && match) { ## 如果pattern不为空，并且match为T
    symbols <- ls(.instrument, all.names = TRUE) ## 列出.instrument环境中所有变量名
    symbols <- symbols[match(pattern, symbols)] ## 获得.instrument环境中与pattern匹配的变量名
  }
  else if (!match && length(pattern) == 1) {
    symbols <- ls(.instrument, all.names = TRUE, pattern = pattern)
  }
  else if (is.null(pattern)) {
    symbols <- ls(.instrument, all.names = TRUE)
  }
  is.iname <- is.instrument.name(symbols)
  if (!any(is.iname)) 
    return(NULL)
  symbols[is.iname]
}
## 下略.
## 此函数返回了.instrument中列出的变量名

## 99%