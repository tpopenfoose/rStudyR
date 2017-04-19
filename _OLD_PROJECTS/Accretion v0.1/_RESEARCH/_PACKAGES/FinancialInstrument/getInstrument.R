#### original
## This function will search the .instrument environment for objects of class type, using first the primary_id and then any identifiers to locate the instrument. Finally, it will try adding 1 and then 2 dots to the beginning of the primary_id to see if an instrument was stored there to avoid naming conflicts.
## 此功能将搜索。对于类类型对象的仪器使用环境，首先primary_id然后任何标识符来定位仪。最后，它会尝试添加1和2点的primary_id开始看一个仪器存储以避免命名冲突。

function (x, Dates = NULL, silent = FALSE, type = "instrument") 
{
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


## analysis

getInstrument <- function (x, Dates = NULL, silent = FALSE, type = "instrument") {
  tmp_instr <- try(get(x, pos = .instrument), silent = TRUE)
  if (inherits(tmp_instr, "try-error") || !inherits(tmp_instr, type)) {
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


## 获得.instrument环境下的x变量
## 50%