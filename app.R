library("httr")
library("shiny")
library("ggplot2")
library("caTools")
library("quantmod")
library("grid")
library("lattice")
library("latticeExtra")
library("corrplot")

source("helpers.r")
source("shared_variables.r")
source("chooser_input.r")

# User interface ----
ui <- navbarPage("U.S. Macroeconomics",
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
                  choices = list("Without transformation" = " ",
                                 "Change" = "diff", 
                                 "% Change" = "rdiff",
                                 "Cumulative" = "cumul"),
                  selected = " ")
    ),
    
    mainPanel(tabsetPanel(
      tabPanel("Price chart", 
               plotOutput("plot", height = "700px"),
               br(),
               fluidRow(column(1),
                        column(10,verbatimTextOutput("stock_summary")),
                        column(1))),
      tabPanel("Macroeconomic indicators", 
               plotOutput("macro_chart", height = "700px"),
               br(),
               fluidRow(column(4),
                        column(4,verbatimTextOutput("macro_summary")),
                        column(4)))))),
    
    tabPanel("Housing market",
             sidebarPanel(
               helpText(strong("Choose indicator and state to analyse. 
                               NOTE: Not every statistic is available for every state.")),
               
               selectizeInput("indicator_housing", "Statistic:", INDICATORS, selected = "DOZP"),
               
               selectizeInput("state", "State:", STATES, selected = 2),
               
               selectizeInput("housing_chart_type", "Choose chart type", choices = 
                                list("Line" = "Line",
                                     "Histogram" = "Histogram", 
                                     "Dots" = "Dots",
                                     "Candles" = "Candles",
                                     "Year groups" = "Year groups"), 
                              selected = "Line"),
               
               dateRangeInput("housing_dates", 
                              "Date range",
                              start = "2010-08-01", 
                              end = as.character(Sys.Date())),
               
               selectInput("housing_chart_transformation", "Choose transformation for second chart:",
                           choices = list("Without transformation" = " ",
                                          "Change" = "diff", 
                                          "% Change" = "rdiff",
                                          "Cumulative" = "cumul"),
                           selected = " ")),
             
             mainPanel(
               plotOutput("housing_chart", height = "700px"))
             ),
  tabPanel("Housing - correlation",
           sidebarPanel(
             helpText("Choose indicators to check correlation. NOTE: Not every coefficient is available for every state."),
             chooserInput("indicators_vector", "Available indicators", "Selected",
                          INDICATORS_NAMES, c(), size = 10, multiple = TRUE),
             br(),
             selectizeInput("corr_state", "State:", STATES, selected = 2),
             br(),
             selectInput("corr_macro_indicator", "Choose macroeconomic indicator to compare:",
                         choices = list("Unemployment (adjusted)" = "unem_lt",
                                        "Unemployment (unadjusted)" = "unem_st", 
                                        "Real Potential GDP" = "rpgdp",
                                        "Nominal Potential GDP" = "npgdp",
                                        "None" = " "),
                         selected = " ")),
           
           mainPanel(plotOutput("corr_chart", height = "700px")))
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
    housing_chart(indicator = input$indicator_housing, state = input$state, chart_type = input$housing_chart_type,
                  min_date = input$housing_dates[1], max_date = input$housing_dates[2], 
                  transformation=input$housing_chart_transformation)
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
  
  output$corr_chart <- renderPlot({
    housing_corelation(state = input$corr_state, indicators_names_vector = input$indicators_vector$right,
                       corr_macro_indicator = input$corr_macro_indicator)
  })
}

# Run the app
shinyApp(ui, server)