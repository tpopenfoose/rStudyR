

#### TEST type 'function'
setClass(
  Class = 'TEST',
  slots = list(test.fun = 'function')
)

setGeneric('fun', function(object) standardGeneric('fun'))
setMethod('fun', signature = (object = 'TEST'), function(object) object@test.fun())

test.fun1 <- function() print('test.fun1!!!')

a <- new('TEST', test.fun = test.fun1)

fun(a)
