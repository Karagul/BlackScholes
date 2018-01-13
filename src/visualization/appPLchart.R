# Title     : Shiny App P/L Chart tab
# Objective : Compartmentalize the app in order to make development of individual parts easier.
# Created by: Tyler
# Created on: 2018-01-13
library(shiny)
library(tidyverse)
library(pracma)

ui <- fluidPage(

  titlePanel("BlackScholes Options Dashboard"),

    mainPanel(
      tabsetPanel(
  # P/L Chart Panel
        tabPanel("P/L Chart", sidebarLayout(
          sidebarPanel(
            radioButtons('stratType', "Strategy Type",
                         choices=c("Bullish","Bearish",
                                   "Neutral","Volatility")),
          uiOutput("strategies")
        ), mainPanel(plotOutput("pl_plot")))),

    )))



server <- function(input, output, session) {

 # P/L Chart Tab
  output$strategies <- renderUI({
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
                choices=select(filter(all_strats, strat_type==input$stratType), Strategy))
  })


  output$plchart <- renderPlot({
    moneyness

  })
}



shinyApp(ui, server)







