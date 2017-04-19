#### original
## Returns TRUE if name corresponds to an argument in the call, either a formal argument to the function, or a component of ..., and FALSE otherwise.
## 如果在调用中的参数对应于函数的一个参数，或者一个…的一个组件的函数，或者一个……的一个组成部分，则返回真的。
function (name) 
{
  aname <- as.character(substitute(name)) ## @substitute:Primitive##返回的解析树的（巨大的）表达式expr，代env约束的任何变量@
  ## @sys.parent():returns the number of the parent frame if n is 1 (the default), the grandparent if n is 2, and so on@
  ## @@
  fnames <- names(formals(sys.function(sys.parent()))) 
  if (is.na(match(aname, fnames))) {
    if (is.na(match("...", fnames))) 
      FALSE
    else {
      dotsCall <- eval(quote(substitute(list(...))), sys.parent())
      !is.na(match(aname, names(dotsCall)))
    }
  }
  else eval(substitute(!missing(name)), sys.frame(sys.parent()))
}

#### analysis