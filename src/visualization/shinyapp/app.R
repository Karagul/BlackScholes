library(shiny)
library(pracma)
library(tidyverse)
library(plot3D)
library(plotly)
library(Hmisc)

priceOption <- function(contract, strike, spot, ttm, vol, rfr){
  # Calculates the theoretical price of a European-style option contract
  # ## SHOULD I JUST FEED D1 INTO THIS TO MINIMIZE CALCULATIONS? ##
  # 
  # Args:
  #
  #   contract (char):
  #   strike (numeric):
  #   spot (numeric):
  #   ttm (numeric):
  #   vol (numeric):
  #   rfr (numeric):
  #
  #
  # Returns:
  #
  #
  #
  #
  d1 <- (1/vol*sqrt(ttm))* (log(spot/strike) + (rfr + ((vol^2)/2))*ttm)
  d2 <- d1 - vol*sqrt(ttm)
  
  if(contract=='call'){
    out <- pnorm(d1)*spot - pnorm(d2)*strike*exp(-rfr*ttm)
    return(out)
  }
  else if(contract=='put'){
    out <- pnorm(-d2)*strike*exp(-rfr*ttm) - pnorm(-d1)*spot
    return(out)
  }
  else{
    return(glue("{capitalize(contract)} is not a valid contract type"))
  }
}



calc_d <- function(grid, strike, vol, rfr){
  # Calculates the d1 portion of the Black Scholes formula
  out <- (1/(vol*sqrt(grid$Y)))*(log(grid$X)/strike) + ((rfr + ((vol^2))*grid$Y))
}

ui <- fluidPage(
  
  titlePanel("BlackScholes Options Dashboard"),
  sidebarLayout(
    sidebarPanel(
      textInput("strike", label = "Strike Price", value = 1),
      textInput("rfr", label = "'Risk-free' Rate", value = 0.05),
      textInput("vol", label = "Volatility", value = 0.13),
      sliderInput('maturity', "Maturity",
                  min=1, max=1095, value = 90,
                  sep="", step=1),
      sliderInput('money_range', "Moneyness",
                  min=0, max=2, value = c(0.5, 1.5),
                  sep="", step=0.1),
      selectInput("greek", "Greek", 
                  choices=c('Delta','Vega', 'Theta', 'Rho', 'Psi'),
                  selected='Delta'),
      radioButtons('contract', label="Contract Type",
                   choices=c("Call","Put"),
                   selected='Call')
      ),

    mainPanel(
      tabsetPanel(
        tabPanel("Greek Surface", plotlyOutput("mainplot")),
        tabPanel("P/L Chart", tableOutput("plchart")),
        tabPanel("Pricing", tableOutput("pricing")),
        tabPanel("Strategy Planner", uiOutput('strategy'))
      )
    )
  
  
))

server <- function(input, output, session) {
  
  
  output$mainplot <- renderPlotly({
    
    greek <- tolower(input$greek)
    strike <- as.numeric(input$strike)
    vol <- as.numeric(input$vol)
    rfr <- as.numeric(input$rfr)
    money_range=c(0.5, 1.5)
    
    
       ## Moneyness will need to be determined based on the contract type I think.
    
      moneyness <-  seq(from=strike*input$money_range[1],
                        to=strike*input$money_range[2],by=(strike*input$money_range[2] - strike*input$money_range[1])/100)
      
      maturities <- seq(from=1, to=input$maturity ,by=1)
      
      grid <- meshgrid(x=moneyness, y=maturities)
      

      d1 <- calc_d(grid, strike, vol, rfr)
      d2 <- d1 - vol*sqrt(grid$Y)
      

      Z <- pnorm(d1)
      

     # layout(add_surface(plot_ly(z = ~Z), x=grid$X, y=grid$Y),
       #        xaxis=list(title="Moneyness"), yaxis=list(title="Maturity"), zaxis=list(title="Delta"))
      plot_ly(z = ~Z) %>% layout(
        title = glue("{capitalize(greek)} Surface"),
        scene = list(
          xaxis = list(title = "Moneyness"),
          yaxis = list(title = "Maturity"),
          zaxis = list(title = glue({greek}))
        )) %>%  add_surface(x=grid$X, y=grid$Y)
  })  
  
  output$plchart <- renderTable({
    moneyness
    
  })
  
  output$pricing <- renderTable({
    
  })
  
  output$strategy <- renderUI({
    
  })
  
}

shinyApp(ui, server)
