require(R6)

R6C_Accretion <- R6Class(
  classname = 'Accretion',
  public = list(
    initialize = function() {
      instruments <- list()
      ## ToDo
    },
    add.instrument = function(instrument.name) {
      if(length(instrument.name) == 1) {
        instrument.name <- as.character(instrument.name)
        if(!instrument.name %in% names(private$instruments)) {
          private$instruments[[instrument.name]] <- private$init.instrument(instrument.name)
        }
      } else {
        sapply(instrument.name, self$add.instrument)
      }
    },
    add.exchange = function(instrument.name, exchange.name) {
      if(length(instrument.name) > 1) {
        return(NULL)
      }
      self$add.instrument(instrument.name)
      get.instruments(instrument.name)$add.exchange(exchange.name)
    },
    add.symbol = function(instrument.name, exchange.name, symbol.name) {
      if(length(instrument.name) > 1 | length(exchange.name) > 1) {
        return(NULL)
      }
      self$add.instrument(instrument.name)
      self$add.exchange(exchange.name)
      get.instrument(instrument.name)$get.exchanges(exchange.name)$add.symbol(symbol.name)
    },
    
    ## Getter and Setter
    get.instrument = function(index) {
      if(missing(index)) return(private$instruments)
      if(length(index) > 1) {
        return(private$instruments[index])
      }
      private$instruments[[index]]
    },
    get.exchange = function(instrument.index, exchange.index) {
      instruments <- self$get.instrument(instrument.index)
      # if()
      ## ToDo 
    },
    get.symbol = function() {
      
      ## ToDo 
    }
  ),
  private = list(
    init.instrument = function(instrument) {
      instrument <- as.character(instrument)
      switch(instrument,
        Forex = R6C_Instrument_Forex$new()
               
         # Stock = R6C_i,
         ## ToDo
      )
    },
    instruments = NULL
    ## ToDo
  )
)