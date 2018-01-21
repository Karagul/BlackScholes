# Title     : Shiny App P/L Chart tab
# Objective : Compartmentalize the app in order to make development of individual parts easier.
# Created by: Tyler
# Created on: 2018-01-13
library(shiny)
library(tidyverse)
library(pracma)
library(shinycssloaders)
source("C:\\Users\\Tyler\\PycharmProjects\\BlackScholes\\src\\utils\\plcharts.R")


ui <- fluidPage(

  titlePanel("BlackScholes Options Dashboard"),

    mainPanel(
      tabsetPanel(
  # P/L Chart Panel
        tabPanel("P/L Chart", sidebarLayout(
          sidebarPanel(
            radioButtons('strat_type', "Strategy Type",
                         choices=c("Bullish","Bearish",
                                   "Neutral","Volatility")),
          uiOutput("strategy_names"),
          uiOutput("contract1"),
          uiOutput("strike1"),
          uiOutput("premium1"),
          
          # This condition must be in JS
          conditionalPanel(condition="(['Bull Call Spread', 'Bull Put Spread', 'Secured Short Put',
            'Married Put', 'Bear Call Spread','Bear Put Spread', 'Short Straddle', 'Short Strangle'].indexOf(input.strategy) >= 0)",
                           radioButtons("contract2","Contract 1 Type",
                                        choices=c("Call","Put")),
                           textInput("strike2", "Strike 2",
                                     value=56),
                           textInput("premium2", "Premium 2",
                                     value=4)
                           )

        ),
        mainPanel(withSpinner(plotOutput("plchart")), verbatimTextOutput("plHelpText"))))

    )))



server <- function(input, output, session) {

 # P/L Chart Tab
  output$strategy_names <- renderUI({
    all_strats <- as.tibble(t(as.data.frame(list(
      c('Bullish', "Long Call"),
      c("Bullish", "Bull Call Spread"),
      c("Bullish", "Bull Put Spread"),
      c("Bullish","Covered Call"),
      c("Bullish","Married Put"),
      c("Bullish","Secured Short Put"),
      c("Bearish","Long Put"),
      c("Bearish","Bear Put Spread"),
      c("Bearish","Bear Call Spread"),
      c("Neutral","Collar"),
      c("Neutral","Short Straddle"),
      c("Neutral","Short Strangle"),
      c("Neutral","Iron Condor"),
      c("Neutral","Calendar Spread"),
      c("Neutral","Covered Strangle"),
      c("Neutral","Long Call Butterfly"),
      c("Volatility","Long Straddle"),
      c("Volatility","Long Strangle"),
      c("Volatility","Call Backspread"),
      c("Volatility","Put Backspread")))))

    names(all_strats) <- c('strat_type', 'Strategy')

    selectInput('strategy', "Strategy",
                choices=select(filter(all_strats, strat_type==input$strat_type),Strategy))
  })

  
  output$contract1 <- renderUI({
    radioButtons("contract1","Contract 1 Type",
                 choices=c("Call","Put"))
  })
  output$strike1 <- renderUI({
    textInput('strike1', "Strike 1",
              value=50)
  })
  output$premium1 <- renderUI({
    textInput('premium1', "Premium 1",
              value=2)
  })
  
  
  output$contract2 <- renderUI({
    radioButtons("contract2", "Contract 2 Type",
              choices=c("Call","Put"))
  })
  output$strike2 <- renderUI({
    textInput("strike2", "Strike 2",
              value=56)
  })
  output$premium2 <- renderUI({
    textInput("premium2", "Premium 2",
              value=4)
  })

  plcharts <- reactive({
    strike1 <- as.numeric(input$strike1)
    strike2 <- as.numeric(input$strike2)
    premium1 <- as.numeric(input$premium1)
    premium2 <- as.numeric(input$premium2)
    price_at_expiry <- c(0:100)
    
    plot_pl(input$strategy, input$contract1, strike1, premium1, 
            input$contract2, strike2, premium2)
      
      
  })
  
  output$plchart <- renderPlot({
    
    strike1 <- as.numeric(input$strike1)
    strike2 <- as.numeric(input$strike2)
    premium1 <- as.numeric(input$premium1)
    premium2 <- as.numeric(input$premium2)
    price_at_expiry <- c(0:100)
    
    plcharts()

    
    
    
  })
  
  output$plHelpText <- renderText({
    print("Currently implemented P/L plots include: Long Call, Covered Call, Secured Short Put, Bull Call Spread, Bull Put Spread, Collar, Bear Call Spread, Bear Put Spread, Short Straddle, and Short Strangle.
          
          These plots are designed to help you outline the profit and loss scenarios for a given position. The idea is that you would use these to get a general idea of the strategy you would like to enter, and then you would use the greek surfaces in combintion with the pricing tool to map out risks, and determine exactly which contracts you would like to buy/sell in order to enter into the given strategy.")
  })

}



shinyApp(ui, server)







