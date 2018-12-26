library("httr")
library("shiny")
library("ggplot2")
library("caTools")
library("quantmod")

source("helpers.r")
source("shared_variables.r")

# User interface ----
ui <- navbarPage("My App",
  # titlePanel("Stocks"),
  tabPanel("Stocks and macro",

    sidebarPanel(
      helpText("Enter the ticker for the stock you want to view."),
      
      textInput("symb", "Symbol", "SPY"),
      
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
      
      br(),
      br(),
      
      dateRangeInput("dates", 
                     "Date range",
                     start = "2010-08-01", 
                     end = as.character(Sys.Date())),
      
      br(),
      br(),
      
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
    
    mainPanel(tabsetPanel(
      tabPanel("Price chart", 
               plotOutput("plot", height = "700px"),
               verbatimTextOutput("stock_summary")),
      tabPanel("Macroeconomic indicators", 
               plotOutput("macro_chart", height = "700px"),
               verbatimTextOutput("macro_summary"))))),
    
    tabPanel("Housing market",
             sidebarPanel(
               helpText(strong("Choose indicator and state to analyse. 
                               NOTE: Not every statistic is available for every state.")),
               
               selectizeInput("indicator_housing", "Statistic:", INDICATORS, selected = "DOZP"),
               
               selectizeInput("state", "State:", STATES, selected = 2)),
             
             mainPanel(
               plotOutput("housing_chart", height = "700px"))
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
  
  output$housing_chart <- renderPlot({
    housing_chart(indicator = input$indicator_housing, state = input$state)
  })
  
  output$stock_summary <- renderPrint({
    data <- dataInput()
    if(input$adjust){
      data <- adjust(dataInput())
    }
    
    summary(data)
  })
  
  output$macro_summary <- renderPrint({
    unem_df <- macro_stats(indicator = input$indicator, 
                min_date = input$dates[1], max_date = input$dates[2], transformation = input$chart_transformation)
    summary(unem_df)
  })
}

# Run the app
shinyApp(ui, server)