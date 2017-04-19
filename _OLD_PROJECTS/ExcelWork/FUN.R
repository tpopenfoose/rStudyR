##

CheckExcleFile <- function(filename) {
  strs <- strsplit(filename, '.', fixed = TRUE)[[1]]
  len <- length(strs)
  if(len == 1) return(FALSE)
  type <- strs[len]
  if(type == 'xlsx' | type == 'xls') return(TRUE)
  FALSE
}
CheckExcleFiles <- function(files) {
  ## ToDo
}