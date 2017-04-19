
rm(list = ls())
library(quantmod)
# library(mcga)
library(rgenoud)

load('./Data/eurusd60.rdata')

f <- function(x) {

  (round(x[1]*100) - 5)^2 + (round(x[2]*100) - 55)^2 + (round(x[3]*100) - 555)^2 + (round(x[4]*100) - 5555)^2 + (round(x[5]*100) - 55555)^2

}
# 
# m <- mcga(  popsize=200,
# chsize=5,
# minval=0.0,
# maxval=999999,
# maxiter=2500,
# crossprob=1.0,
# mutateprob=0.01,
# evalFunc=f)

# RSI.Gene <- function(x, dt = t.EURUSD) {
#   
# }

# m <- genoud(f, pop.size = 200, max.generations = 2500, nvars = 5, max = F)

# m <- genoud(f, pop.size = 600, max.generations = 2500, nvars = 5, max = F, lexical = T, print.level = 0)
# m <- optim(c(1,1,1,1,1), f)
print(m)


# RSI(t.EURUSD$Close, n = 14.5)
