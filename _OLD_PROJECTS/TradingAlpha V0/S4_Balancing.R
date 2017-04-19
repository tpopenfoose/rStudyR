
library(caret)

Balancing <- function(c.dt, b.ratio = 1.05) {
  output.table <- table(c.dt$OUTPUT)
  ratio <- max(output.table) / min(output.table)
  if(ratio <= b.ratio) {
    c.dt <- coredata(c.dt)
    input <- subset(c.dt, select = -OUTPUT)
    output <- c.dt[, 'OUTPUT']
  } else {
    data.x <- subset(c.dt, select = -OUTPUT)
    data.y <- as.factor(c.dt$OUTPUT)
    balanced.data <- upSample(x = data.x, y = data.y, yname = 'OUTPUT')
    input <- subset(balanced.data, select = -OUTPUT)
    output <- as.numeric(as.character(balanced.data$OUTPUT))
  }
  list(
    Input = input,
    Output = output
  )
}