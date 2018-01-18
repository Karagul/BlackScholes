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
            radioButtons('strat_type', "Strategy Type",
                         choices=c("Bullish","Bearish",
                                   "Neutral","Volatility")),
          uiOutput("strategy_names"),
          uiOutput("contract1"),
          uiOutput("strike1"),
          uiOutput("premium1"),
          # This condition must be JS
          conditionalPanel(condition="if (['Bull Call Spread', 'Bull Put Spread', 'Covered Call', 'Secured Short Put',
                           'Married Put', 'Bear Call Spread','Bear Put Spread', 'Short Straddle', 'Short Strangle'].indexOf(input.strategy_names) >= 0",
                           textInput("strike2", "Strike 2",
                                     value=56),
                           textInput("premium2", "Premium 2",
                                     value=56)
                           )

        ),
        mainPanel(plotOutput("plchart"))))

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
    radioButtons("contract 1","Contract 1 Type",
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

  plcharttest <- reactive({
    if(input$strategy=="Long Call"){
      strike1 <- as.numeric(input$strike1)
      strike2 <- as.numeric(input$strike2)
      premium1 <- as.numeric(input$premium1)
      premium2 <- as.numeric(input$premium2)
      price_at_expiry <- c(0:100)
      
      g <- ggplot(x=price_at_expiry) +
        geom_hline(yintercept=0, color='red') +
        geom_segment(aes(x=0, xend=strike1, y=-premium1, yend=-premium1)) +
        # This yend=48 needs to be properly coded. Currently just hardcoded for this graph.
        geom_segment(aes(x=strike1,xend=100,y=-premium1,yend=48)) +
        geom_segment(aes(x=strike1,
                         xend=strike1,
                         y=0,
                         yend=-premium1),
                     linetype="dotted") +
        
        xlab("Stock Price at Expiry") +
        ylab("Profit") +
        coord_cartesian(xlim=c(40,60),ylim=c(-5,5)) +
        scale_y_continuous(labels=dollar) +
        ggtitle("Long Call P/L Chart") +
        theme_bw()
      
      return(g)
      
    }
  })
  
  output$plchart <- renderPlot({

    plcharttest
    
    
    
  })
}



shinyApp(ui, server)







