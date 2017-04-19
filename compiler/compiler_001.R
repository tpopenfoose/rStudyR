# 结论

library(compiler)

la1 <- function(X, FUN, ...) {
  FUN <- match.fun(FUN)
  if (!is.list(X))
    X <- as.list(X)
  rval <- vector("list", length(X))
  for(i in seq(along = X))
    rval[i] <- list(FUN(X[[i]], ...))
  names(rval) <- names(X)		  # keep `names' !
  return(rval)
}

la1c <- cmpfun(la1)

la1cc <- function(X, FUN, ...) la1(X, FUN, ...)

la1ccc <- cmpfun(la1cc)

x <- 1:100

system.time(for (i in 1:10000) la1(x, is.null))
system.time(for (i in 1:10000) la1c(x, is.null))
system.time(for (i in 1:10000) la1ccc(x, is.null))
