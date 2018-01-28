# Title     : Shiny App P/L Chart tab
# Objective : Compartmentalize the app in order to make development of individual parts easier.
# Created by: Tyler
# Created on: 2018-01-13
library(shiny)
library(tidyverse)
library(pracma)
library(shinycssloaders)
library(glue)
source("C:\\Users\\Tyler\\PycharmProjects\\BlackScholes\\src\\utils\\plcharts.R")


ui <- fluidPage(

  titlePanel("BlackScholes Options Dashboard"),

    mainPanel(
      tabsetPanel(
  # P/L Chart Panel
        tabPanel("P/L Chart", 
          sidebarLayout(
            sidebarPanel(
              radioButtons('strat_type', "Strategy Type",
                           choices=c("Bullish","Bearish",
                                     "Neutral","Volatility")),
            uiOutput("strategy_names"),
            uiOutput("strike1"),
            uiOutput("premium1"),
            
            
            # This condition must be in JS
            conditionalPanel(condition="(['Bull Call Spread', 'Bull Put Spread',
                                          'Bear Call Spread','Bear Put Spread', 'Short Straddle', 'Short Strangle','Iron Condor',
                       'Calendar Spread', 'Covered Strangle', 'Long Call Butterfly','Long Straddle', 'Long Strangle', 'Call Backspread', 'Put Backspread'].indexOf(input.strategy) >= 0)",
                             uiOutput("strike2"),
                             uiOutput("premium2")
                             )
  
          ),
        mainPanel(withSpinner(plotOutput("plchart")), htmlOutput("plHelpText"))
        )

    ))))



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
  
  
  output$strike1 <- renderUI({
    textInput('strike1', "Strike 1",
              value=50)
  })
  output$premium1 <- renderUI({
    textInput('premium1', "Premium 1",
              value=2)
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
    
    plot_pl(input$strategy, contract1="call", strike1, premium1, 
            contract2="call", strike2, premium2)
    
  })
  
  output$plchart <- renderPlot({
    
    strike1 <- as.numeric(input$strike1)
    strike2 <- as.numeric(input$strike2)
    premium1 <- as.numeric(input$premium1)
    premium2 <- as.numeric(input$premium2)
    price_at_expiry <- c(0:100)
    
    plcharts()
    
    
    
    
  })
  
  output$plHelpText <- renderUI({
    
    
    str1 <- glue("Currently implemented P/L plots include: <br/> Long Call, Covered Call, Secured Short Put, Married Put, Bull Call Spread, Bull Put Spread, Collar, Bear Call Spread, Bear Put Spread, Short Straddle, and Short Strangle.")
    
    typehelp <- glue("Below is a guide to some of the UI elements. <br/> <br/> <b>Strategy Type:</b> These are four broad types of options strategy. They filter the shown options below.")
    
    strathelp <- glue("<b>Strategy:</b> As of yet, the implemented strategies either require 1 or 2 contracts. An additional element will appear to specify the second contract if necessary.")
    
    strikehelp <- glue("<b>Strike:</b> The strike prices for each of the contracts. In strategies which require more than one contract, Strike 1 is always the lower strike price. Inputs should be numeric, and relatively proportional to each other.")
    
    premiumhelp <- glue("<b>Premium:</b> The premium received or paid to enter each of the contracts. This is analogous to the price of the option. Inputs should be numeric and are generally between 0 and 10")
    
    
    
    HTML(paste(str1, typehelp, strathelp, strikehelp, premiumhelp, sep="<br/> <br/>"))
  })
  
  plinfotext <- reactive({
    x <- pl_help(input$strategy)
  })
  
  output$pl_info <- renderUI({
    plinfotext()
  })

}



shinyApp(ui, server)







