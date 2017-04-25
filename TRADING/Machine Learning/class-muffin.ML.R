############################################################
# Title:    machine learning for trading base class
# Date:     2017-04-21
# Version:  0.1
############################################################

library(R6)

muffin.ML <- R6Class(
  classname = 'Machine Learning for Trading',
  public = list(
    ## initialize
    initialize = function(symbol, timeframe, model.file.dir) {
      private$symbol <- symbol
      private$timeframe <- timeframe
      private$model.file.path <- model.file.path(model.file.dir, symbol, timeframe,
                                                 private$file.extension) %T>% print
      if (file.exists(private$model.file.path)) {
        self$load.model.file()
      }
    },
    ## virtual
    input.price.data = function(price.data) {
      ## TODO
      private$price.data %<>%
        rbind(price.data %>% price.data.add.median, use.names = TRUE, fill = TRUE)
      private$input %<>% private$fun.input(private$price.data)
      private$output %<>% private$fun.output(private$price.data)
    },
    build.model = function() {
      ## TODO
      
    },
    predict = function() {
      ## TODO
      
    },
    ## getter & setter
    get = function(member) {
      private[[member]]
    },
    set = function(member, value) {
      private[[member]] <- value
    },
    ## save & load file
    load.model.file = function(members=private$save.members, file=private$model.file.path) {
      env <- environment()
      load(file = file)
      sapply(members, function(member) {
        private[[member]] <- get(member, envir = env)
      })
    },
    save.model.file = function(members=private$save.members, file=private$model.file.path) {
      env <- environment()
      sapply(members, function(member) {
        assign(member, private[[member]], envir = env)
      })
      save(list = members, file = file)
    }
  ),
  private = list(
    # bars.for.build.model = 1000,
    ## virtual
    file.extension = 'mml',
    save.members = NULL,
    ## member
    symbol = NULL,
    timeframe = NULL,
    model.file.path = NULL,
    
    price.data = NULL,
    input = NULL,
    output = NULL,
    
    model = NULL,
    fun.input = fun.input.default,
    fun.output = fun.output.default
    
  )
)


