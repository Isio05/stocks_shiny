library("httr")
library("shiny")
library("ggplot2")
library("caTools")

source("helpers.r")
source("shared_variables.r")

# User interface ----
ui <- fluidPage(
  titlePanel("Stocks"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Enter the ticker for the stock you want to view."),
      
      textInput("symb", "Symbol", "SPY"),
      
      dateRangeInput("dates", 
                     "Date range",
                     start = "2010-08-01", 
                     end = as.character(Sys.Date())),
      
      tags$h5(strong("Additional plot features")),
      
      checkboxInput("log", "Plot y axis on log scale", 
                    value = FALSE),
      
      checkboxInput("adjust", 
                    "Adjust prices for inflation", value = FALSE),
      
      radioButtons("price_indicator", label = ("Add indicator to price chart"),
                   choices = list("MACD" = "MACD", "RSI" = "RSI", "TDI" = "TDI", "None" = "None"), 
                   selected = "None"),
      
      selectInput("price_chart_type", "Choose chart type:",
                  choices = list("Candles" = "candlesticks",
                                 "Match sticks" = "matchsticks", 
                                 "Classic bars" = "bars",
                                 "Line" = "line")),
      
      selectInput("indicator", "Choose macroeconomic indicator:",
                  choices = list("Unemployment (adjusted)" = "unem_lt",
                                 "Unemployment (unadjusted)" = "unem_st", 
                                 "Real Potential GDP" = "rpgdp",
                                 "Nominal Potential GDP" = "npgdp"),
                  selected = " "),
      
      selectInput("chart_transformation", "Choose transformation for second chart:",
                  choices = list("Without transform" = " ",
                                 "Change" = "diff", 
                                 "% Change" = "rdiff",
                                 "Cumulative" = "cumul"),
                  selected = " ")
    ),
    
    mainPanel(plotOutput("plot"),
              br(),
              br(),
              plotOutput("macro_chart"))
  )
)

# Server logic
server <- function(input, output) {
  
  dataInput <- reactive({
    getSymbols(input$symb, src = "yahoo", 
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
  finalOutput <- reactive({
    
  })
  
  
  output$plot <- renderPlot({
    data <- dataInput()
    if(input$adjust){
      data <- adjust(dataInput())
    }
    
    chartSeries(data, theme = chartTheme(theme = "white", bg.col="white", major.tick="black", minor.tick="white"),
                type = input$price_chart_type, log.scale = input$log, TA = NULL, name = input$symb)
    
    if(input$price_indicator == "MACD"){
      addMACD()      
    }else if(input$price_indicator == "RSI"){
      addRSI()
    }else if(input$price_indicator == "TDI"){
      addTDI()
    }
  })
    
  output$macro_chart <- renderPlot({
    macro_chart(indicator = input$indicator, 
      min_date = input$dates[1], max_date = input$dates[2], transformation = input$chart_transformation)
  })
    
  output$stock_summary <- renderPrint({
    summary(data)
  })
}

# Run the app
shinyApp(ui, server)