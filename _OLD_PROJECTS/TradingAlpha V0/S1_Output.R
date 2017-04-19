# Output <- function(market, method = 'ZigZag', ch = 0.00370) { ## sell: -1 版本 ## 可扩展，添加method或者args
#   out <- ZigZag(market[ ,c('High', 'Low')], change = ch, percent = F, retrace = F, lastExtreme = T)
#   out.diff <- diff(out)
#   sign(out.diff)
# }

Output <- function(market, method = 'ZigZag', ch = 0.00370) {  ## sell: 0 版本 ## 可扩展，添加method或者args
  out <- ZigZag(market[ ,c('High', 'Low')], change = ch, percent = F, retrace = F, lastExtreme = T)
  out.diff <- diff(out)
  sign(out.diff >= 0)
}

Simulator <- function(market, signals = c(1)) {
  close <- market$Close
  diff.close <- diff(close)
  diff.close[1] <- 0
  diff.close
  # signals <- signals[-length(signals)]
  # diff.close * signals
}