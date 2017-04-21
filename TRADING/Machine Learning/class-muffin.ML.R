############################################################
# Title:    machine learning for trading base class
# Date:     2017-04-21
# Version:  0.1
############################################################

library(R6)

muffin.ML <- R6Class(
  classname = 'Machine Learning for Trading',
  public = list(
    
  ),
  private = list(
    fun.input = NULL,
    fun.output = NULL,
    model = NULL
  )
)
