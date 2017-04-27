############################################################
# Title:    Stacked Auto-Encoder for trading [muffin.ML]
# Date:     2017-04-21
# Version:  0.1
############################################################

library(R6)
library(deepnet)

muffin.ML.SAE <- R6Class(
  classname = 'Stacked Auto-Encoder',
  inherit = muffin.ML,
  public = list(
    
  ),
  private = list(
    file.extension = 'sae'
  )
)


## functions
Model.SAE <- function(input, output, hidden=c(50, 50, 50), activationfun='tanh',
                      learningrate=0.7, momentum=0.5, learningrate_scale=1, output='sigm',
                      sae_output='linear', numepochs=100, batchsize=50, hidden_dropout=0,
                      visible_dropout=0) {
  sae.dnn.train(x=input, y=output, hidden=hidden, activationfun=activationfun,
                learningrate=learningrate, momentum=momentum, learningrate_scale=learningrate_scale,
                output=output, sae_output=sae_output, numepochs=numepochs, batchsize=batchsize,
                hidden_dropout=hidden_dropout, visible_dropout=visible_dropout)
}

