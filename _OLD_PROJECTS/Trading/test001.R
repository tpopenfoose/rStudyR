library(DMwR)


trading.simulator.ori <- function (market, signals, policy.func, policy.pars = list(), trans.cost = 5, init.cap = 1e+06) {
  open.position <- function(type, prc, quant) {
    .currPos <<- .currPos + 1
    if (.currPos > .maxPos) {
      n <- .maxPos%/%4
      .maxPos <<- .maxPos + n
      positions <<- rbind(positions, matrix(NA, nrow = n, 
                                            ncol = 7, dimnames = list(.currPos:.maxPos, c("pos.type", 
                                                                                          "N.stocks", "Odate", "Oprice", "Cdate", "Cprice", 
                                                                                          "result"))))
    }
    positions[.currPos, ] <<- c(pos.type = type, N.stocks = quant, 
                                Odate = d, Oprice = prc, Cdate = NA, Cprice = NA, 
                                result = NA)
    trading[d, "Order"] <<- type
    trading[d, "N.Stocks"] <<- trading[d, "N.Stocks"] + type * 
      quant
    trading[d, "Money"] <<- trading[d, "Money"] - type * 
      quant * prc - trans.cost
    if (trading[d, "Money"] < 0) 
      cat("Borrowing money (", abs(trading[d, "Money"]), 
          ") for opening a long position (PosID=", .currPos, 
          ")\n")
    open.positions <<- c(open.positions, .currPos)
    return(.currPos)
  }
  close.position <- function(ID, prc) {
    quant <- positions[ID, "N.stocks"]
    value <- positions[ID, "pos.type"] * quant * prc
    -trans.cost
    trading[d, "Order"] <<- -positions[ID, "pos.type"]
    trading[d, "Money"] <<- trading[d, "Money"] + positions[ID, 
                                                            "pos.type"] * quant * prc - trans.cost
    if (trading[d, "Money"] < 0) 
      cat("Borrowing money (", abs(trading[d, "Money"]), 
          ") for closing a short position (PosID=", ID, 
          ")\n")
    trading[d, "N.Stocks"] <<- trading[d, "N.Stocks"] - positions[ID, 
                                                                  "pos.type"] * quant
    positions[ID, "Cdate"] <<- d
    positions[ID, "Cprice"] <<- prc
    init <- if (positions[ID, "pos.type"] == 1) 
      positions[ID, "Oprice"]
    else positions[ID, "Cprice"]
    fin <- if (positions[ID, "pos.type"] == 1) 
      positions[ID, "Cprice"]
    else positions[ID, "Oprice"]
    positions[ID, "result"] <<- 100 * (fin/init - 1)
    open.positions <<- open.positions[-which(open.positions == 
                                               ID)]
  }
  dates <- index(market)
  market <- as.data.frame(market)
  N.days <- nrow(market)
  res <- list()
  trading <- matrix(0, nrow = N.days, ncol = 5)
  colnames(trading) <- c("Close", "Order", "Money", "N.Stocks", 
                         "Equity")
  trading[, "Close"] <- market$Close
  trading[1, "Money"] <- init.cap
  .maxPos <- N.days%/%2
  positions <- matrix(NA, nrow = .maxPos, ncol = 7, dimnames = list(1:.maxPos, 
                                                                    c("pos.type", "N.stocks", "Odate", "Oprice", "Cdate", 
                                                                      "Cprice", "result")))
  .currPos <- 0
  open.positions <- c()
  pending.orders <- NULL
  .orderID <- 1
  for (d in 1:N.days) {
    if (d > 1) {
      trading[d, "Money"] <- trading[d - 1, "Money"]
      trading[d, "N.Stocks"] <- trading[d - 1, "N.Stocks"]
    }
    if (NROW(pending.orders)) {
      mkts <- which(pending.orders$order.type == 1)
      closed <- c()
      for (i in mkts) {
        if (pending.orders[i, "action"] == "open") {
          idP <- open.position(pending.orders[i, "order"], 
                               market[d, "Open"], pending.orders[i, "val"])
          pending.orders[pending.orders$ID == pending.orders[i, 
                                                             "ID"], "posID"] <- idP
        }
        else {
          close.position(pending.orders[i, "posID"], 
                         market[d, "Open"])
          closed <- c(closed, pending.orders[i, "posID"])
        }
      }
      if (length(mkts)) {
        pending.orders <- pending.orders[-mkts, ]
        toRem <- which(pending.orders$posID %in% closed)
        if (length(toRem)) 
          pending.orders <- pending.orders[-toRem, ]
      }
      if (NROW(pending.orders)) {
        done <- c()
        for (i in 1:NROW(pending.orders)) {
          if (!i %in% done) {
            if (pending.orders[i, "order.type"] == 2) {
              if (pending.orders[i, "order"] == 1) {
                if (market[d, "Low"] < pending.orders[i, 
                                                      "val"]) {
                  close.position(pending.orders[i, "posID"], 
                                 pending.orders[i, "val"])
                  done <- c(done, which(pending.orders$posID == 
                                          pending.orders[i, "posID"]))
                }
              }
              else {
                if (market[d, "High"] > pending.orders[i, 
                                                       "val"]) {
                  close.position(pending.orders[i, "posID"], 
                                 pending.orders[i, "val"])
                  done <- c(done, which(pending.orders$posID == 
                                          pending.orders[i, "posID"]))
                }
              }
            }
            else if (pending.orders[i, "order.type"] == 
                     3) {
              if (pending.orders[i, "order"] == 1) {
                if (market[d, "High"] > pending.orders[i, 
                                                       "val"]) {
                  close.position(pending.orders[i, "posID"], 
                                 pending.orders[i, "val"])
                  done <- c(done, which(pending.orders$posID == 
                                          pending.orders[i, "posID"]))
                }
              }
              else {
                if (market[d, "Low"] < pending.orders[i, 
                                                      "val"]) {
                  close.position(pending.orders[i, "posID"], 
                                 pending.orders[i, "val"])
                  done <- c(done, which(pending.orders$posID == 
                                          pending.orders[i, "posID"]))
                }
              }
            }
          }
        }
        if (length(done)) 
          pending.orders <- pending.orders[-done, ]
      }
    }
    orders <- do.call(policy.func, c(list(signals[1:d], market[1:d, 
                                                               ], if (.currPos) positions[open.positions, 1:4, drop = F] else NULL, 
                                          trading[d, "Money"]), policy.pars))
    if (no <- NROW(orders)) 
      orders <- cbind(ID = rep(.orderID, no), orders)
    .orderID <- .orderID + NROW(orders)
    pending.orders <- rbind(pending.orders, orders)
    trading[d, "Equity"] <- trading[d, "Money"] + trading[d, 
                                                          "N.Stocks"] * market[d, "Close"]
  }
  trading <- zoo(trading, dates)
  tradeRecord(trading, if (.currPos) 
    positions[1:.currPos, , drop = FALSE]
    else as.matrix(vector()), trans.cost, init.cap, policy.func, 
    policy.pars)
}