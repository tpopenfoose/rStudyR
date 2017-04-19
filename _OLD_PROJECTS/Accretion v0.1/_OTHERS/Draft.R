# test.001 <- function(n) {
  # v <- 1:n
  # for(i in 1:n)
  #   v[i] <- v[i]^2
# }
# 
# print(system.time(test.001(1000000)))
# 
# require(compiler)
# test.001.compiled <- cmpfun(test.001)
# print(system.time(test.001.compiled(1000000)))
# 
# 
# abc <- cmpfun(function(x) {x^2})
# 
# test.001.compiled.2 <- cmpfun(test.001.compiled)
# print(system.time(test.001.compiled.2(1000000)))

require(R6)


ABC <- R6Class(
  classname = '',
  public = list(
    AA = function(n) {
      private$aaa(n)
    },
    BB = function(n) private$bbb(n)
  ),
  private = list(
    aaa = function(n) {
      test.001.compiled.2(n)
    },
    bbb = function(n) {
      v <- 1:n
      for(i in 1:n)
        v[i] <- v[i]^2
      print(ls(parent.env(environment())))
    }
    
  )
)
aa <- ABC$new()
print(system.time(aa$AA(1000000)))
print(system.time(aa$BB(1000000)))
