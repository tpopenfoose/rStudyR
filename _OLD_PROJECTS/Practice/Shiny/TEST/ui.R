library(shiny)

shinyUI(fluidPage(

  verticalLayout(
    titlePanel("MT4 Report Analyzer"),
    mainPanel(
      tabsetPanel(
        tabPanel("TRADE",  DT::dataTableOutput("TRADE")), 
        tabPanel("SYMBOL", DT::dataTableOutput("SYMBOL")), 
        tabPanel("ACCOUNT", DT::dataTableOutput("ACCOUNT"))
      )
    ),
    #plotOutput("plot1"),
    wellPanel(
      sliderInput("n", "Number of points", 10, 200,
                  value = 50, step = 10)
    )
  )
))
