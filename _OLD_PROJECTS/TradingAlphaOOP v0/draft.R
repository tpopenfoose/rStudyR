


abc <- function(a) {
  print(a)
  print(length(a))
}

a <- c('1', '2', '3')

lapply(a, abc)
