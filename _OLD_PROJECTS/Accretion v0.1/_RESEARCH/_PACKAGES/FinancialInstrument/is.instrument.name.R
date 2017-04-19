#### analyse
function (x) {
  if (!is.character(x)) {
    return(F)
  }
  sapply(lapply(x, getInstrument, silent = TRUE), inherits, "instrument") ## @getInstrument@
}

## 检查名字是否存在于instrument环境中











#### original
function (x) 
{
  if (!is.character(x)) 
    return(FALSE)
  sapply(lapply(x, getInstrument, silent = TRUE), inherits, 
         "instrument")
}