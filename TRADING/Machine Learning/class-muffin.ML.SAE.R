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
# sae.dnn.train()

while