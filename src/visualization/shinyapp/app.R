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
                   radioButtons('contract', label="Contract Type",
                                choices=c("Call","Put"),
                                selected='Call'),
                   
                   textInput("rfr", label = "'Risk-free' Rate", value = 0.05),
                   
                   textInput("vol", label = "Volatility", value = 0.13),
                   
                   sliderInput('maturity', "Maturity",
                               min=15, max=365, value = 90,
                               sep="", step=15),
                   
                   sliderInput('money_range', "Moneyness",
                               min=0, max=2, value = c(0.5, 1.5),
                               sep="", step=0.1),
                   
                   radioButtons('greekOrder', "Order",
                                choices=c("First","Second","Third"),
                                selected="First"),
                   
                   uiOutput("greekType")
                   
                 ),
                 mainPanel(withSpinner(plotlyOutput("greekplot", width="150%", height="175%")), htmlOutput("surfaceHelp")))
      ),
      # P/L Chart Panel
      tabPanel("P/L Chart", sidebarLayout(
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
      mainPanel(withSpinner(plotOutput("plchart")), htmlOutput("plHelpText"))))
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
    vol <- as.numeric(input$vol)
    rfr <- as.numeric(input$rfr)
    money_range=c(input$money_range[1], input$money_range[2])
    
    greekSurface(order=input$greekOrder, greek=greek, contract=contract, strike=1, vol=vol, rfr=rfr,
                 maturity=input$maturity, money_range=money_range)$Z
  })
  
  
  
  output$greekplot <- renderPlotly({
    
    greek <- tolower(input$greek)
    vol <- as.numeric(input$vol)
    rfr <- as.numeric(input$rfr)
    money_range=c(input$money_range[1], input$money_range[2])
    
    
    ## Moneyness will need to be determined based on the contract type I think, unless I can find a way to just change labels and
    ## not the values underlying the calculation.
    
    moneyness <-  seq(from=input$money_range[1],
                      to=input$money_range[2],by=(input$money_range[2] - input$money_range[1])/100)
    
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
  
  output$surfaceHelp <- renderUI({
    basic <- glue("The above surface is the {input$greek} surface for a {input$contract}. <br/> <br/> If the plot appears blank, try clicking and dragging the area. Please use the buttons at the top of the plot to rotate, pan, and reset the camera if you get lost.") 
    
    contracthelp <- glue("Below is a guide to some of the interactive elements: <br/> <br/> <b>Contract:</b> Whether the contract specified above is a Call or a Put. Please note that not all greeks are different for Calls and Puts. This is to be expected and is not a bug.")
    
    rfrhelp <- ("<b>Risk Free Rate:</b> This is the cost of carrying these contracts. Input should be numeric; a ratio that represents the interest rate per annum.")
    
    volhelp <- glue("<b>Volatility:</b> This is the standard deviation of stock moves in percent terms. Inputs should be between 0 and 1. The 'average' volatility of stock prices is around 0.13")
    
    mathelp <- glue("<b>Maturity:</b> This is measured in days until expiry. This affects one of the axis limits.")
    
    moneyhelp <- glue("<b>Moneyness:</b> This is a measurement of 'how profitable' an option contract is. Can think of it as how many times more or less the spot price is than the strike price, depending on the position taken. This affects the other axis' limits")
    
    orderhelp <- glue("<b>Greek Order:</b> Can think of this as derivaive order. Will impact which greeks are selectable.")
    
    
    HTML(paste(basic, contracthelp, rfrhelp, volhelp, mathelp, moneyhelp, orderhelp, sep="<br/> <br/>"))
    
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
  
  output$plHelpText <- renderUI({
    
    
    str1 <- glue("Currently implemented P/L plots include: <br/> Long Call, Covered Call, Secured Short Put, Married Put, Bull Call Spread, Bull Put Spread, Collar, Bear Call Spread, Bear Put Spread, Short Straddle, and Short Strangle.")
    
    typehelp <- glue("Below is a guide to some of the UI elements. <br/> <br/> <b>Strategy Type:</b> These are four broad types of options strategy. They filter the shown options below.")
    
    strathelp <- glue("<b>Strategy:</b> As of yet, the implemented strategies either require 1 or 2 contracts. An additional element will appear to specify the second contract if necessary.")
    
    strikehelp <- glue("<b>Strike:</b> The strike prices for each of the contracts. In strategies which require more than one contract, Strike 1 is always the lower strike price. Inputs should be numeric, and relatively proportional to each other.")
    
    premiumhelp <- glue("<b>Premium:</b> The premium received or paid to enter each of the contracts. This is analogous to the price of the option. Inputs should be numeric and are generally between 0 and 10")
    
    
    
    HTML(paste(str1, typehelp, strathelp, strikehelp, premiumhelp, sep="<br/> <br/>"))
  })
}
  



shinyApp(ui, server)













