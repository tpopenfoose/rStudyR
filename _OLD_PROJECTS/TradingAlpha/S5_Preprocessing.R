
library(rminer)

Preprocessing <- function(b.dt, s.ratio = 0.8) {
  test <- holdout(b.dt$Output, ratio = s.ratio, mode = 'random')
  test.tr <- test$tr
  test.ts <- test$ts
  input <- b.dt$Input
  pre <- preProcess(input[test.tr, ], method = "spatialSign")
print(head(input[test.tr, ]))
# print(pre)
  input.tr <- predict(pre, input[test.tr, ])
#   input.ts <- predict(pre, input[test.ts, ])
}