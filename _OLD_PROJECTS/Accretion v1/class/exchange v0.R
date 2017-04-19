
R6C_Exchange <- R6Class(
  classname = 'Exchange',
  public = list(
    initialize = function(exchange) {
      ex <- private$exchanges(exchange)
      private$name <- ex$Name
      private$region <- ex$Region
      private$timezone <- ex$TimeZone
      private$rule <- ex$Rule
      ## ToDo
    },
    
    ## Getter and Setter
    getName = function() private$name,
    getRegion = function() private$region,
    getTimeZone = function() private$timezone,
    getRule = function() private$rule
    
  ),
  private = list(
    exchanges = function(exchange) NULL,
    name = NULL,
    region = NULL,
    timezone = NULL,
    rule = NULL
    ## ToDo
  )
)

R6C_Exchange_Forex <- R6Class(
  classname = 'Exchange_Forex',
  inherit = R6C_Exchange,
  public = list(
    initialize = function(exchange) {
      super$initialize(exchange)
      
      ## ToDo
    }
    
  ),
  private = list(
    exchanges = function(exchange) {
      switch (exchange,
        'IC Markets' = list(
          Name = 'IC Markets',
          Region = 'Australia',
          TimeZone = '+2', ## server timezone, not country timezone
          Rule = list(
            Hedge.Allowed = T,
            Direction = c('B', 'S'),
            T.Plus = 0
          )
        ),
        'Oanda' = list(
          Name = 'Oanda',
          Region = 'USA',
          TimeZone = 'unknow', ## server timezone, not country timezone
          Rule = list(
            Hedge.Allowed = F,
            Direction = c('B', 'S'),
            T.Plus = 0
          )
        )
      )
    }
    
    ## ToDo
  )
)