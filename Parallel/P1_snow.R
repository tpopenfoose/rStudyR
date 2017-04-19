library(snow)
rm(list = ls())
cl <- makeCluster(4, type = 'SOCK')


#### Basics ####
library(MASS)

## + Normal ####
time_1 <- system.time({
  result_1 <- kmeans(Boston, 4, nstart=100)
})
print(time_1) ## RESULT:0.03 Secs

## + lapply ####
time_2 <- system.time({
  results <- lapply(rep(25, 4), function(nstart) kmeans(Boston, 4, nstart=nstart))
  i <- sapply(results, function(result) result$tot.withinss)
  result_2 <- results[[which.min(i)]]
})
print(time_2) ## RESULT:0.10 Secs

## + clusterApply ####
time_3 <- system.time({
  ignore <- clusterEvalQ(cl, {library(MASS); NULL})
  results <- clusterApply(cl, rep(25, 4), function(nstart) kmeans(Boston, 4, nstart=nstart))
  i <- sapply(results, function(result) result$tot.withinss)
  result_3 <- results[[which.min(i)]]
})
print(time_3) ## RESULT:0.09 Secs


#### clusterApply VS clusterApplyB ####

## + clusterApplyLB ####
time_4 <- system.time({
  set.seed(7777442)
  sleeptime <- abs(rnorm(10, 10, 10))
  tm <- snow.time(clusterApplyLB(cl, sleeptime, Sys.sleep))
  plot(tm)
})
print(time_4) ## RESULT: 28.95 Secs

## + clusterApply ####
time_5 <- system.time({
  set.seed(7777442)
  sleeptime <- abs(rnorm(10, 10, 10))
  tm <- snow.time(clusterApply(cl, sleeptime, Sys.sleep))
  plot(tm)
})
print(time_5) ## RESULT: 54.43 Secs

#### clusterApply VS parLapply ####
bigsleep <- function(sleeptime, mat) Sys.sleep(sleeptime)
bigmatrix <- matrix(0, 2000, 2000)
sleeptime <- rep(1, 100)

## + clusterApply ####
time_6 <- system.time({
  tm <- snow.time(clusterApply(cl, sleeptime, bigsleep, bigmatrix))
  plot(tm)
})
print(time_6) ## RESULT: 47.75 Secs

## + parLapply ####
time_7 <- system.time({
  tm <- snow.time(parLapply(cl, sleeptime, bigsleep, bigmatrix))
  plot(tm)
})
print(time_7) ## RESULT: 26.25 Secs

## + lapply ####
time_8 <- system.time({
  tm <- snow.time(lapply(sleeptime, bigsleep, bigmatrix))
  plot(tm)
})
print(time_8) ## RESULT: 101.01 Secs

#### clusterSplit ####
parVapply <- function(cl, x, fun, ...) {
  do.call("c", clusterApply(cl, clusterSplit(cl, x), fun, ...))
}
time_9 <- system.time({
  tm <- snow.time(parVapply(cl, 1:10, "^", 1/3))
  plot(tm)
})
print(time_9) ## RESULT: 0.27 Secs

time_10 <- system.time({
  tm <- snow.time(sapply(1:10, "^", 1/3))
  plot(tm)
})
print(time_10) ## RESULT: 0.16 Secs

time_9_1 <- system.time({
  tm <- snow.time(parVapply(cl, 1:1000000, "^", 1/3))
  plot(tm)
})
print(time_9_1) ## RESULT: 2.64 Secs

time_10_1 <- system.time({
  tm <- snow.time(sapply(1:1000000, "^", 1/3))
  plot(tm)
})
print(time_10_1) ## RESULT: 7.71 Secs
## NOTE ####
## 1 for large number of vector parVapply is better than sapply

#### Environments ####

## use clusterExport ####
a <- 1:4
x <- rnorm(4)
# clusterExport(cl, "x")
mult <- function(s) s * x
parLapply(cl, a, mult)

## 
a <- 1:4
mult <- function(s) s * rnorm(4)
parLapply(cl, a, mult)

##
a <- 1:4
x <- function(y) rnorm(y)
# clusterExport(cl, "x")
mult <- function(s) s * x(4)
parLapply(cl, a, mult)

##
a <- 1:4
fun.env <- new.env()
fun.env$x <- function(y) rnorm(y)
clusterExport(cl, 'fun.env')
mult <- function(s) {
  fun <- get('x', envir = fun.env)
  print(fun)
  s * fun(4)
}
# print(lapply(a, mult))
parLapply(cl, a, mult)

##
a <- 1:4
fun.env <- new.env()
library(R6)
env.class <- R6Class(
  public = list(
    test = function(x) {
      private$abc(x)
    }
  ),
  private = list(
    abc = function(x) {
      rnorm(x)
    }
  )
)
fun.env$x <- env.class
clusterExport(cl, 'fun.env')
mult <- function(s) {
  cla <- get('x', envir = fun.env)
  print(cla)
  a <- cla$new()
  s * a$test(4)
}
# print(lapply(a, mult))
parLapply(cl, a, mult)
