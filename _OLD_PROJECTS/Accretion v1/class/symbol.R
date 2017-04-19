require(R6)

R6C_Symbol <- R6Class(
  classname = 'Symbol',
  public = list(
    initialize = function() {
      NULL
      ## ToDo
    },
    get.name = function() private$name,
    get.datas = function() private$datas
  ),
  private = list(
    # instrument = NULL,
    # exchange = NULL,
    name = NULL,
    datas = NULL#,
    
    ## ToDo
  )
)

R6C_Symbol_Forex <- R6Class(
  classname = 'Symbol_Forex',
  inherit = R6C_Symbol,
  public = list(
    initialize = function(symbol.name, datas) {
      super$initialize()
      # private$instrument <- 'forex'
      # private$exchange <- exchange
      private$name <- symbol.name
      private$datas <- datas
      ## ToDo
    }
    
  ),
  private = list(
    
    ## ToDo
  )
)