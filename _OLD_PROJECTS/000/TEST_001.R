library(shiny)
## ui
ui <- shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
    fileInput('file', 'Select a MT4 Report File:', multiple = TRUE, accept = c('text/html'))
    ),
    mainPanel()
  )
))
## server
server <- shinyServer(function(input, output) {
  
})
## App
shinyApp(ui = ui, server = server)