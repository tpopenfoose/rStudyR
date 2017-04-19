Clean <- function(input, output) {
  na.omit(cbind(input, OUTPUT = lag(output, -1)))
}