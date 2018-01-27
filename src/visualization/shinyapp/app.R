# ------------------------------ Imports and Sourcing ------------------------------------- #

library(shiny)
library(shinyjs)
library(pracma)
library(tidyverse)
library(plotly)
library(Hmisc)
library(glue)
library(shinycssloaders)

source("utils/greeks.R")
source("utils/pricing.R")
source("utils/plcharts.R")
source("utils/GreekSurface.R")

# ---------------------------------- App Code ---------------------------------- #


ui <- fluidPage(
  
  titlePanel("BlackScholes Options Dashboard"),
  
  mainPanel(
    tabsetPanel(
      # Greek Surface Tab
      tabPanel("Greek Surface",
               sidebarLayout(
                 sidebarPanel(
                   textInput("strike", label = "Strike Price", value = 1),
                   textInput("rfr", label = "'Risk-free' Rate", value = 0.05),
                   textInput("vol", label = "Volatility", value = 0.13),
                   sliderInput('maturity', "Maturity",
                               min=1, max=365, value = 45,
                               sep="", step=15),
                   sliderInput('money_range', "Moneyness",
                               min=0, max=2, value = c(0.5, 1.5),
                               sep="", step=0.1),
                   uiOutput("greekType"),
                   radioButtons('greekOrder', "Order",
                                choices=c("First","Second","Third"),
                                selected="First"),
                   radioButtons('contract', label="Contract Type",
                                choices=c("Call","Put"),
                                selected='Call')
                 ),
                 mainPanel(withSpinner(plotlyOutput("greekplot", width="150%", height="175%")), verbatimTextOutput("surfaceHelp")))
      ),
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
          conditionalPanel(condition="(['Bull Call Spread', 'Bull Put Spread',
                                        'Bear Call Spread','Bear Put Spread', 'Short Straddle', 'Short Strangle','Iron Condor',
                     'Calendar Spread', 'Covered Strangle', 'Long Call Butterfly','Long Straddle', 'Long Strangle', 'Call Backspread', 'Put Backspread'].indexOf(input.strategy) >= 0)",
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
  # GREEK SURFACE TAB
  output$greekType <- renderUI({
    
    possible_greeks <- as.tibble(t(as.data.frame(list(
      c('First', "Delta"),
      c('First', "Vega"),
      c('First', "Theta"),
      c('First',"Rho"),
      c("Second","Gamma"),
      c("Second","Vanna"),
      c("Second","Charm"),
      c("Second","Vomma"),
      c("Second","Veta"),
      c("Third","Colour"),
      c("Third","Zomma"),
      c("Third","Thega"),
      c("Third","Speed"),
      c("Third","Ultima")))))
    
    names(possible_greeks) <- c('order', 'greek')
    
    selectInput("greek", "Greek",
                choices=filter(possible_greeks, 
                               order==input$greekOrder)$greek)
  })
  
  react_z <- reactive({
    contract <- tolower(input$contract)
    greek <- tolower(input$greek)
    strike <- as.numeric(input$strike)
    vol <- as.numeric(input$vol)
    rfr <- as.numeric(input$rfr)
    money_range=c(input$money_range[1], input$money_range[2])
    
    greekSurface(order=input$greekOrder, greek=greek, contract=contract, strike=strike, vol=vol, rfr=rfr,
                 maturity=input$maturity, money_range=money_range)$Z
  })
  
  
  
  output$greekplot <- renderPlotly({
    
    greek <- tolower(input$greek)
    strike <- as.numeric(input$strike)
    vol <- as.numeric(input$vol)
    rfr <- as.numeric(input$rfr)
    money_range=c(input$money_range[1], input$money_range[2])
    
    
    ## Moneyness will need to be determined based on the contract type I think, unless I can find a way to just change labels and
    ## not the values underlying the calculation.
    
    moneyness <-  seq(from=strike*input$money_range[1],
                      to=strike*input$money_range[2],by=(strike*input$money_range[2] - strike*input$money_range[1])/100)
    
    maturities <- seq(from=1, to=input$maturity ,by=1)
    
    grid <- meshgrid(x=moneyness, y=maturities)
    
    
    d1 <- calc_d(grid, strike, vol, rfr)
    d2 <- d1 - vol*sqrt(grid$Y)
    
    plot_ly(z = ~react_z()) %>% layout(
      title = glue("{capitalize(greek)} Surface"),
      scene = list(
        xaxis = list(title = "Moneyness"),
        yaxis = list(title = "Maturity"),
        zaxis = list(title = glue("{capitalize(greek)}"))
      )) %>%  add_surface(x=grid$X, y=grid$Y) 
  }) 
  
  output$surfaceHelp <- renderText({
    print(glue("The above surface is the {input$greek} surface for a {input$contract} option.


      In a later version of this app, there will be details regarding the interpretation of 
      the above surface, and how it relates to other greek surfaces, along with some smaller 
      plots of lower order greeks if applicable."))
  })
  
  
  
  # P/L CHART TAB
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
    print("Currently implemented P/L plots include: Long Call, Covered Call, Secured Short Put,
          Married Put, Bull Call Spread, Bull Put Spread, Collar, Bear Call Spread, 
          Bear Put Spread, Short Straddle, and Short Strangle.
          
          These plots are designed to help you outline the profit and loss scenarios for a given position. 

          The idea is that you would use these to get a general idea of the strategy you would like to enter,
          and then you would use the greek surfaces in combintion with the pricing tool to map out risks, 
          and determine exactly which contracts you would like to buy/sell in order to enter into the given strategy.")
  })
  
}
  



shinyApp(ui, server)













