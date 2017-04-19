require(R6)
require(compiler)
XXX <- cmpfun(function() print('TESTFUN'))

R6C_CLASSNAME <- R6Class(
  classname = 'CLASSNAME',
  public = list(
    initialize = function() {
      ## ToDo
    },
    TTT = XXX
  ),
  private = list(
    ## ToDo
  )
)


ABC <- R6C_CLASSNAME$new()
ABC$TTT
print(XXX)
XXX
