### 2017-04-19
## yahoo weather page: https://www.yahoo.com/news/weather/


### 2017-04-18
## OS.TYPE
.Platform$OS.type
## for package writing
# add `` in DESCRIPTION file to compiler all functions, this method is for S4(S3?), but not for R6

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
