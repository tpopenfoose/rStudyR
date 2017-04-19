
R6C_Exchange <- R6Class(
  classname = 'Exchange',
  public = list(
    initialize = function() {
      private$symbols <- list()
      NULL
      ## ToDo
    },
    add.symbol = function(symbol.name)  {
      symbol.name <- as.character(symbol.name)
      if(length(symbol.name) == 1) {
        datas <- private$get.datas(symbol.name)
        new.symbol <- private$symbol.class$new(symbol.name, datas)
        private$symbols[[symbol.name]] <- new.symbol
      } else {
        sapply(symbol.name, self$add.symbol)
      }
      ## ToDo
    },
    
    ## Getter and Setter
    get.name = function() private$name,
    get.region = function() private$region,
    get.timezone = function() private$timezone,
    get.rule = function() private$rule,
    get.symbols = function() private$symbols
    
  ),
  private = list(
    get.datas = function(symbol.name) {
      
    },
    name = NULL,
    region = NULL,
    timezone = NULL,
    rule = NULL,
    symbol.class = NULL,
    symbols = NULL
    ## ToDo
  )
)

R6C_Exchange_Forex <- R6Class(
  classname = 'Exchange_Forex',
  inherit = R6C_Exchange,
  public = list(
    initialize = function() {
      super$initialize()
      private$symbol.class <- R6C_Symbol_Forex
      ## ToDo
    }
    
  ),
  private = list(
    
    ## ToDo
  )
)

R6C_Exchange_Forex_ICMarkets <- R6Class(
  classname = 'Exchange_Forex_ICMarkets',
  inherit = R6C_Exchange_Forex,
  public = list(
    initialize = function() {
      super$initialize()
      private$name <- 'IC Markets'
      private$region <- 'Australia'
      private$timezone <- '+2'
      private$rule <- list(
        Hedge.Allowed = T,
        Direction = c('B', 'S'),
        T.Plus = 0
      )
    }
  ),
  private = list(
    get.datas = function(symbol.name) {
      ## test code {
      get(load(file = './DRAFT/EURUSD.xts_60.rdata'))
      ## test code }
      
    }
    
  )
)


R6C_Exchange_Forex_Oanda <- R6Class(
  classname = 'Exchange_Forex_Oanda',
  inherit = R6C_Exchange_Forex,
  public = list(
    initialize = function() {
      super$initialize()
      private$name <- 'Oanda'
      private$region <- 'USA'
      private$timezone <- 'unknow'
      private$rule <- list(
        Hedge.Allowed = F,
        Direction = c('B', 'S'),
        T.Plus = 0
      )
    }
  ),
  private = list(
    ## ToDo
  )
)