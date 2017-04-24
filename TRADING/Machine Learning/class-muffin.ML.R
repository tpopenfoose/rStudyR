############################################################
# Title:    machine learning for trading base class
# Date:     2017-04-21
# Version:  0.1
############################################################

library(R6)

muffin.ML <- R6Class(
  classname = 'Machine Learning for Trading',
  public = list(
    initialize = function(symbol, timeframe, model.file.dir) {
      private$symbol <- symbol
      private$timeframe <- timeframe
      private$model.file.path <- model.file.path(model.file.dir, symbol, timeframe,
                                                 private$file.extension)
      if (file.exists(private$model.file.path)) {
        private$load.model.file()
      }
    },
    
    get = function(member) {
      private[[member]]
    },
    set = function(member, value) {
      private[[member]] <- value
    }
  ),
  private = list(
    file.extension = NULL,
    
    symbol = NULL,
    timeframe = NULL,
    model.file.path = NULL,
    
    model = NULL,
    fun.input = NULL,
    fun.output = NULL,
    
    load.model.file = function() {
      #### TODO
    },
    save.model.file = function() {
      #### TODO
    }
  )
)
