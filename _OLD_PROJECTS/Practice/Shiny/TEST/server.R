library(shiny)

shinyServer(function(input, output, session) {

  output$TRADE <- DT::renderDataTable({
    DT::datatable(RESULT$TRADE_TABLE[,1:(dim(RESULT$TRADE_TABLE)[2]-4)])
  })
  
  output$SYMBOL <- DT::renderDataTable({
    DT::datatable(RESULT$SYMBOL_TABLE)
  })
  output$ACCOUNT <- DT::renderDataTable({
    DT::datatable(RESULT$PRICE_TABLE[[1]])
  })
})
