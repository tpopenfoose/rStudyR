
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
    },
    save.file3 = function() {
      env <- environment()
      sapply(private$save.members, function(member) {
        assign(member, private[[member]], envir = env)
      })
      save(list = private$save.members, file = private$file)
    },
    load.file3 = function() {
      env <- environment()
      load(file = private$file)
      sapply(private$save.members, function(member) {
        private[[member]] <- get(member, envir = env)
      })
    }
  ),
  private = list(
    save.members = c('A', 'B'),
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
ttt$save.file3()
ttt$set('A', 5)
ttt$set('B', 15)
ttt$get('A')
ttt$get('B')
ttt$load.file3()
## private$A as symbol cannot be saved