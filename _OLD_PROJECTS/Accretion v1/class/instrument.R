
R6C_Instrument <- R6Class(
  classname = 'Instrument',
  public = list(
    initialize = function() {
      private$exchanges <- list()
      
      ## ToDo
    },
    add.exchange = function(exchange) {
      if(length(exchange) == 1) {
        exchange <- as.character(exchange)
        if(!exchange %in% names(private$exchanges)) {
          private$exchanges[[exchange]] <- private$init.exchange(exchange)}
      } else {
        sapply(exchange, self$add.exchange)
      }
    },
    add.symbol = function(exchange, symbol.name) {
      if(length(exchange) > 1) {
        return(NULL)
      }
      self$add.exchange(exchange)
      self$get.exchange(exchange)$add.symbol(symbol.name)
    },
    
    ## Getter and Setter
    get.type = function() private$type,
    get.exchange = function(index) {
      if(missing(index)) return(private$exchanges)
      if(length(index) > 1) {
        return(private$exchanges[index])
      }
      private$exchanges[[index]]
    }
    
  ),
  private = list(
    init.exchange = function(exchange) {
      NULL
    },
    type = NULL, ## 类型：股票，期货，外汇 etc.
    exchanges = NULL ## 交易所：上海，深圳，Oanda等
    ## ToDo
  )
)


R6C_Instrument_Forex <- R6Class(
  classname = 'Instrument_Forex',
  inherit = R6C_Instrument,
  public = list(
    initialize = function() {
      super$initialize()
      private$type <- 'forex'
      ## ToDo
    }
    
  ),
  private = list(
    init.exchange = function(exchange) {
      exchange <- as.character(exchange)
      switch (exchange,
        ICMarkets = R6C_Exchange_Forex_ICMarkets$new(),
        Oanda = R6C_Exchange_Forex_Oanda$new()
      )
    }
    ## ToDo
  )
)