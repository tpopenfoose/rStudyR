rm(list = ls())

## FUNCTIONS 
getInstrument <- function (x, Dates = NULL, silent = FALSE, type = "instrument") {  ## from: FinancialInstrument金融工具
  # This function will search the .instrument environment for objects of class type, 
  # using first the primary_id and then any identifiers to locate the instrument. Finally, 
  # it will try adding 1 and then 2 dots to the beginning of the primary_id to see if an instrument was stored there to avoid naming conflicts.
  # 此功能将搜索。对于类类型对象的仪器使用环境，首先primary_id然后任何标识符来定位仪。最后，它会尝试添加1和2点的primary_id开始看一个仪器存储以避免命名冲突。
  tmp_instr <- try(get(x, pos = .instrument), silent = TRUE)
  if (inherits(tmp_instr, "try-error") || !inherits(tmp_instr, 
                                                    type)) {
    xx <- make.names(x)
    ul.instr <- unlist(as.list(.instrument, all.names = TRUE))
    ul.ident <- ul.instr[grep("identifiers", names(ul.instr))]
    tmpname <- ul.ident[ul.ident %in% unique(c(x, xx))]
    if (length(tmpname) > 0) {
      id <- gsub("\\.identifiers.*", "", names(tmpname))
      tmp_instr <- try(get(id, pos = .instrument), silent = TRUE)
      if (inherits(tmp_instr, type)) {
        return(tmp_instr)
      }
    }
    char.x <- strsplit(x, "")[[1]]
    x <- substr(x, grep("[^\\.]", char.x)[1], length(char.x))
    tmp_instr <- try(get(x, pos = .instrument), silent = TRUE)
    if (!inherits(tmp_instr, type)) {
      tmp_instr <- try(get(paste(".", x, sep = ""), pos = .instrument), 
                       silent = TRUE)
      if (!inherits(tmp_instr, type)) {
        tmp_instr <- try(get(paste("..", x, sep = ""), 
                             pos = .instrument), silent = TRUE)
      }
    }
    if (inherits(tmp_instr, type)) 
      return(tmp_instr)
    if (!silent) 
      warning(paste(type, x, "not found, please create it first."))
    return(FALSE)
  }
  else {
    return(tmp_instr)
  }
}

currency <- function (primary_id, identifiers = NULL, assign_i = TRUE, ...) { ## from: FinancialInstrument
  # All 'currency' instruments must be defined before instruments of other types may be defined.
  # 所有的“货币”工具必须在其他类型的工具之前定义。
  if (hasArg("overwrite")) {
    if (!list(...)$overwrite && isTRUE(assign_i) && any(in.use <- primary_id %in% 
                                                        (li <- ls_instruments()))) {
      stop(paste(paste("In currency(...) : ", "overwrite is FALSE and primary_id", 
                       if (sum(in.use) > 1) 
                         "s are"
                       else " is", " already in use:\n", sep = ""), 
                 paste(intersect(primary_id, li), collapse = ", ")), 
           call. = FALSE)
    }
  }
  if (length(primary_id) > 1) {
    out <- sapply(primary_id, currency, identifiers = identifiers, 
                  assign_i = assign_i, ... = ..., simplify = assign_i)
    return(if (assign_i) unname(out) else out)
  }
  if (is.null(identifiers)) 
    identifiers <- list()
  ccy <- try(getInstrument(primary_id, type = "currency", silent = TRUE))
  if (is.instrument(ccy)) {
    if (length(identifiers) > 0) {
      if (!is.list(identifiers)) 
        identifiers <- list(identifiers)
      for (nm in names(ccy$identifiers)[names(ccy$identifiers) %in% 
                                        names(identifiers)]) {
        ccy$identifiers[[nm]] <- identifiers[[nm]]
      }
      identifiers <- identifiers[names(identifiers)[!names(identifiers) %in% 
                                                      names(ccy$identifiers)]]
      ccy$identifiers <- c(identifiers, ccy$identifiers)
    }
  }
  else ccy <- list(primary_id = primary_id, currency = primary_id, 
                   multiplier = 1, tick_size = 0.01, identifiers = identifiers, 
                   type = "currency")
  dargs <- list(...)
  if (!is.null(dargs)) {
    for (nm in names(ccy)[names(ccy) %in% names(dargs)]) {
      ccy[[nm]] <- dargs[[nm]]
    }
    dargs <- dargs[names(dargs)[!names(dargs) %in% names(ccy)]]
    ccy <- c(ccy, dargs)
  }
  class(ccy) <- c("currency", "instrument")
  if (assign_i) {
    assign(primary_id, ccy, pos = as.environment(.instrument))
    return(primary_id)
  }
  ccy
}
currency('USD')


#### 

