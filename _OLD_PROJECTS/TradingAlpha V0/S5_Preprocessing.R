
library(rminer)
library(deepnet)

Preprocessing <- function(b.dt, s.ratio = 0.8) {
  input <- b.dt$Input
  output <- b.dt$Output
  output.ho <- holdout(output, ratio = s.ratio, mode = 'random')
  output.tr <- output.ho$tr
  output.ts <- output.ho$ts
  input.pre <- preProcess(input[output.tr, ], method = 'spatialSign') ## 将input的变量值规范到[0, 1]或者[-1, 1] @ToDo检验其他method
  input.tr <- predict(input.pre, input[output.tr, ])
  input.ts <- predict(input.pre, input[output.ts, ])
  list(
    Prepr = input.pre,
    Input = input, ## 不必要
    Output = output, ## 不必要
    Input.Tr = input.tr,
    Input.Ts = input.ts,
    Output.Tr = output[output.tr],
    Output.Ts = output[output.ts]
  )
}