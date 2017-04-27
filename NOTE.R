## 2017-04-27
# deepnet包有bug，分类的结果时正时反


## 2017-04-21
library(R6)

TEST <- R6Class(
  public = list(
    TEST.FUN = function(n) naive.vector.of.squares(n)
  )
)
naive.vector.of.squares <- function(n) {
  v <- 1:n
  for (i in 1:n) {
    v[i] <- v[i]^2
  }
}

# with `ByteCompile: true` in DESCRIPTION, compiled; without, not 

### 2017-04-19
## yahoo weather page: https://www.yahoo.com/news/weather/


### 2017-04-18
## OS.TYPE
.Platform$OS.type
## for package writing
# add `ByteCompile: true` in DESCRIPTION file to compiler all functions, this method is for S4(S3?), but not for R6

## R6 methods compiler
ABC <- R6Class(
public = list(
  TEST.FUN1 = hello,
  TEST.FUN2 = function() {
    print('TEST.FUN2')
  },
  TEST.FUN3 = cmpfun(hello)
)
)

ABC$set('public', 'TEST.FUN4', function() print('HALLO!'))
# all above could not compiler methods Orz

## names
# muffin for finance, financial
# murlock for machine learning
# quantitative investment
