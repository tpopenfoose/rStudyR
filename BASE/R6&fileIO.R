
library(R6)


TEST.R6 <- R6Class(
  public = list(
    
    load.file = function() {
      load(private$file)
      private$A <- a
      private$B <- xxx
    },
    save.file = function() {
      save(private$A, private$B, private$C, file = private$file)
    },
    save.file2 = function(a = private$C) {
      print(ls(private))
      save(a, file = private$file)
    },
    get = function(member) {
      private[[member]]
    },
    set = function(member, value) {
      private[[member]] <- value
    }
  ),
  private = list(
    file = 'test.r6_fileio.rdata',
    A = 0,
    B = 1,
    C = 'a'
    
  )
)
ttt <- TEST.R6$new()
ttt$save.file()
ttt$save.file2()
ttt$load.file()
ttt$get('A')
## private$A as symbol cannot be saved