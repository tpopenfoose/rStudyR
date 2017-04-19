

Testing <- function(model, data.clean, bars = 0) {
  rows <- nrow(data.clean)
  if(bars > rows | bars == 0) bars <- rows
  
  input <- subset(data.clean, select = -OUTPUT)
  output <- data.clean[, 'OUTPUT']
  
  prepr <- model$Prepr
  
  input.ts <- tail(input, bars)
  input.ts <- predict(prepr, input.ts)
  output.ts <- tail(output, bars)
# print(input.ts)
  # sae.pred <- nn.predict()
  # do.call(nn.predict, list(model$Models, input.ts))
  input.pred <- nn.predict(model$Models[[1]][[1]], input.ts)
  input.pred.mean <- mean(input.pred)
  sig <- ifelse(input.pred > input.pred.mean, 1, 0)
# print(head(sig))
  print(confusionMatrix(output.ts, sig))
  sig.x <- ifelse(sig == 1, 1, -1)
# print(head(sig.x))
  price <- cbind(t.EURUSD, Sig = sig.x)
  price <- na.omit(price)
  close <- coredata(price$Close)
  diff.close <- diff(close)
  sig.x <- sig.x[-length(sig.x)]
print(head(diff.close))
  balance <- diff.close * sig.x
  
print(length(which(balance > 0)))
print(length(which(balance < 0)))
# print(head(balance))
  balance <- cumsum(balance)
}