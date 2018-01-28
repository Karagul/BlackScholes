# Title     : Shiny App's Greek Surface Tab
# Objective : Compartmentalize the app in order to make development of individual parts easier.
# Created by: Tyler
# Created on: 2018-01-13
library(shiny)
library(tidyverse)
library(pracma)
library(plotly)
library(shinycssloaders)
library(glue)
library(Hmisc)


source("C:\\Users\\Tyler\\PycharmProjects\\BlackScholes\\src\\utils\\greeks.R")

source("C:\\Users\\Tyler\\PycharmProjects\\BlackScholes\\src\\utils\\GreekSurface.R")


ui <- fluidPage(

  titlePanel("BlackScholes Options Dashboard"),

    mainPanel(
      tabsetPanel(

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
        )
      )))



server <- function(input, output, session) {
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
    
    
    d1 <- calc_d(grid, 1, vol, rfr)
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
    
    volhelp <- glue("<b>Volatility:</b> This is the standard deviation of stock moves in percent terms. Inputs should be between 0 and 1. The 'average' volatility of stock prices is around 0.13. In general, volatility will increase the value of an options contract.")
    
    mathelp <- glue("<b>Maturity:</b> This is measured in days until expiry. This affects one of the axis limits.")
    
    moneyhelp <- glue("<b>Moneyness:</b> This is a measurement of 'how profitable' an option contract is. Can think of it as how many times more or less the spot price is than the strike price, depending on the position taken. This affects the other axis' limits")
    
    orderhelp <- glue("<b>Greek Order:</b> Can think of this as derivaive order. Will impact which greeks are selectable.")
    
    
    HTML(paste(basic, contracthelp, rfrhelp, volhelp, mathelp, moneyhelp, orderhelp, sep="<br/> <br/>"))
    
  })
  
}


shinyApp(ui, server)
