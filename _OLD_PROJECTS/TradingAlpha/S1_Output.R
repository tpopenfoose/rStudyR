Output <- function(market, method = 'ZigZag', ch = 0.00370) { ## 可扩展，添加method或者args
  out <- ZigZag(market[ ,c('High', 'Low')], change = ch, percent = F, retrace = F, lastExtreme = T)
  out.diff <- diff(out)
  sign(out.diff)
}