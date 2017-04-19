#### original
## All 'currency' instruments must be defined before instruments of other types may be defined.
## 所有的“currency”工具必须在其他类型的工具之前定义。
{
  function (primary_id, identifiers = NULL, assign_i = TRUE, ...) 
  {
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
    ccy <- try(getInstrument(primary_id, type = "currency", 
                             silent = TRUE))
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
}

## analysis

currency <- function (primary_id, identifiers = NULL, assign_i = TRUE, ...) {
  if (hasArg("overwrite")) { ## 如果参数中含有overwrite
    if (!list(...)$overwrite && isTRUE(assign_i) && any(in.use <- primary_id %in% (li <- ls_instruments()))) { ## @ls_instruments ## overwrite为F,并且assign_i为T, primary_id存在于.instrument环境中
      stop(paste(paste("In currency(...) : ", "overwrite is FALSE and primary_id", 
                       if (sum(in.use) > 1) 
                         "s are"
                       else " is", " already in use:\n", sep = ""), 
                 paste(intersect(primary_id, li), collapse = ", ")), 
           call. = FALSE)
    }
  }
  if (length(primary_id) > 1) {
    out <- sapply(primary_id, currency, identifiers = identifiers, assign_i = assign_i, ... = ..., simplify = assign_i)
    return(if (assign_i) unname(out) else out)
  }
  if (is.null(identifiers)) {
    identifiers <- list()
  }
  ccy <- try(getInstrument(primary_id, type = "currency", silent = TRUE)) ## @getInstrument
  if (is.instrument(ccy)) { ## @is.instrument
    if (length(identifiers) > 0) {
      if (!is.list(identifiers)) identifiers <- list(identifiers)
      for (nm in names(ccy$identifiers)[names(ccy$identifiers) %in% names(identifiers)]) {
        ccy$identifiers[[nm]] <- identifiers[[nm]]
      }
      identifiers <- identifiers[names(identifiers)[!names(identifiers) %in% names(ccy$identifiers)]]
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