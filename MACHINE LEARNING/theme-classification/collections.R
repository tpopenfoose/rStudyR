library(R6)
library(magrittr)

CLASSIFICATION <- R6Class(
  public = list(
    initialize = function() {
      private$train.input <- random.input()
      private$train.output <- c(rep(1, 50), rep(0, 50))
      private$test.input <- random.input()
    },
    
    TEST = function() {
      self$predict() %T>% print
    },
    
    train = function(train.input=private$train.input, train.output=private$train.output) {
      private$model <- private$p.train(train.input, train.output)
    },
    
    predict = function(model=private$model, test.input=private$test.input) {
      if (private$model %>% is.null) {
        self$train(private$train.input, private$train.output)
      }
      private$p.predict(model, test.input)
    },
    
    get = function(member) {
      private[[member]]
    },
    set = function(member, value) {
      private[[member]] <- value
    }
  ),
  private = list(
    model = NULL,
    train.input = NULL,
    train.output = NULL,
    test.input = NULL,
    test.output = NULL#,
    
    # p.train = NULL,
    # p.predict = NULL
  )
)



random.input <- function() {
  cbind(F1 = c(rnorm(50, 1, 0.5), rnorm(50, -0.6, 0.2)), 
        F2 = c(rnorm(50, -0.8, 0.2), rnorm(50, 2, 1)))
}


#### nnet
library(nnet)
CLASSIFICATION.nnet <- R6Class(
  inherit = CLASSIFICATION,
  public = list(
    
  ),
  private = list(
    p.train = function(train.input, train.output) {
      nnet(train.input, train.output, size = 2, rang = 0.1, decay = 5e-4, maxit = 200)
    },
    p.predict = function(model, test.input) {
      predict(model, test.input, type = 'raw')
    }
  )
)

#### nnet2 formula
# must data.frame
# class must be factor
library(nnet)
CLASSIFICATION.nnet2 <- R6Class(
  inherit = CLASSIFICATION,
  public = list(
    
  ),
  private = list(
    p.train = function(train.input, train.output) {
      data <- data.frame(train.input, OUTPUT = as.character(train.output)) %T>% str
      nnet(OUTPUT ~ ., data = data, size = 2, rang = 0.1, decay = 5e-4, maxit = 200)
    },
    p.predict = function(model, test.input) {
      predict(model, test.input, type = 'class')
    }
  )
)
## TEST
# TEST <- CLASSIFICATION$new()
# TEST$get('train.input')
# TEST$get('test.input')
# TEST <- CLASSIFICATION.nnet$new()
TEST <- CLASSIFICATION.nnet2$new()
TEST$get('model')
TEST$TEST() %T>% plot
