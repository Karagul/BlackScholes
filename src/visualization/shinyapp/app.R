library(shiny)
library(shinyjs)
library(pracma)
library(tidyverse)
library(plot3D)
library(plotly)
library(Hmisc)
library(glue)


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
  #   out (numeric): Black Scholes theoretical price for a Europoean-style option.
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

implied_vol <- function(price, contract, spot, ttm){
  # Approximates the implied volatility of an option, given its price and other standard parameters.
  #
  # Args:
  #   price (numeric):
  #   previous_vol (numeric):
  #   contract (char):
  #   strike (numeric):
  #   spot (numeric):
  #   ttm (numeric):
  #   rfr (numeric):
  #
  # Returns:
  #   vol
  #
  # Notes:
  #   Approximation formula from Brenner and Subrahmanyam (1988)
  #   Will be updated to make use of the improved approximation from Corrado & Miller (1996), or Stefanica and Radoicic (2017).
  #
  vol <- (sqrt((2*pi)/ttm)* price/spot)
  return(vol)
}


calc_d <- function(grid, strike, vol, rfr){
  # Calculates the d1 portion of the Black Scholes formula
  out <- (1/(vol*sqrt(grid$Y)))*(log(grid$X)/strike) + ((rfr + ((vol^2))*grid$Y))
}

ui <- fluidPage(
  
  titlePanel("BlackScholes Options Dashboard"),

    mainPanel(
      tabsetPanel(
        
        # Greek Surface Panel
        tabPanel("Greek Surface", 
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
                 mainPanel(plotlyOutput("greekplot", width="150%", height="175%")))
                  ),
        
        # P/L Chart Panel
        tabPanel("P/L Chart", tableOutput("plchart")),
        
        # Pricing Panel
        tabPanel("Pricing", sidebarLayout(
          sidebarPanel(
            textInput("spot", "Spot Price",
                      value = 50),
            textInput("ttm", "Time to Maturity (Years)",
                      value = 50),
            textInput("strike", "Strike Price",
                      value = 50),
            textInput("rfr", "Spot Price",
                      value = 50),
            radioButtons('pricing_contract', "Contract Type",
                         choices=c("Call","Put")),
            uiOutput("priceInput"),
                         
            uiOutput("impVol")
          ),
          mainPanel(plotlyOutput("volsurface", width="150%", height="175%"))),
                 
                 tableOutput("pricing")),
        
        
        tabPanel("Strategy Planner", uiOutput('strategy'))
      )
    )
  
  
)

server <- function(input, output, session) {
  
  # Greek Surface Tab
  output$greekplot <- renderPlotly({
    
    greek <- tolower(input$greek)
    strike <- as.numeric(input$strike)
    vol <- as.numeric(input$vol)
    rfr <- as.numeric(input$rfr)
    money_range=c(0.5, 1.5)
    
    
      ## Moneyness will need to be determined based on the contract type I think, unless I can find a way to just change labels and
      ## not the values underlying the calculation.
    
      moneyness <-  seq(from=strike*input$money_range[1],
                        to=strike*input$money_range[2],by=(strike*input$money_range[2] - strike*input$money_range[1])/100)
      
      maturities <- seq(from=1, to=input$maturity ,by=1)
      
      grid <- meshgrid(x=moneyness, y=maturities)
      

      d1 <- calc_d(grid, strike, vol, rfr)
      d2 <- d1 - vol*sqrt(grid$Y)
      

      Z <- pnorm(d1)
      

      plot_ly(z = ~Z) %>% layout(
        title = glue("{capitalize(greek)} Surface"),
        scene = list(
          xaxis = list(title = "Moneyness"),
          yaxis = list(title = "Maturity"),
          zaxis = list(title = glue({greek}))
        )) %>%  add_surface(x=grid$X, y=grid$Y)
  })  
  
  # P/L Chart Tab
  output$plchart <- renderTable({
    moneyness
    
  })
  
  # Pricing Tab
  
  # Goal is to have pricing/implied vol update when the other is changed.
  # Maybe add a reload button?
  output$priceInput <- renderUI({
    pricing_contract <- tolower(input$pricing_contract)
    spot <- as.numeric(input$spot)
    ttm <- as.numeric(input$ttm)
    strike <- as.numeric(input$strike)
    rfr <- as.numeric(input$rfr)
    
    tagList(
      
      textInput("optionPrice", "Option Price",
                  value = priceOption(pricing_contract, strike, spot, ttm, vol, rfr))
     )})
  
  output$impVol <- renderUI({
    pricing_contract <- tolower(input$pricing_contract)
    spot <- as.numeric(input$spot)
    ttm <- as.numeric(input$ttm)
    strike <- as.numeric(input$strike)
    rfr <- as.numeric(input$rfr)
    optionPrice <- as.numeric(input$optionPrice)
    
    tagList(
      
    textInput("pricing_vol", "(Implied) Volatility",
              value=implied_vol(optionPrice, pricing_contract, spot, ttm))
  )})
  
  # Implied volatility surface
  output$volsurface <- renderPlotly({
    pricing_contract <- tolower(input$pricing_contract)
    optionPrice <- as.numeric(input$optionPrice)
    maturities <-  seq(from=1, to=100, by=1)
    price_range <- seq(from=-optionPrice*3, to=optionPrice*3,
                       by=0.1)
    grid <- meshgrid(x=price_range, y=maturities)
    spot <- as.numeric(input$spot)
    
    Z <- implied_vol(grid$X, pricing_contract, spot, grid$Y)
    
    # layout(add_surface(plot_ly(z = ~Z), x=grid$X, y=grid$Y),
    #        xaxis=list(title="Moneyness"), yaxis=list(title="Maturity"), zaxis=list(title="Delta"))
    plot_ly(z = ~Z) %>% layout(
      title = "Implied Volatility Surface",
      scene = list(
        xaxis = list(title = "Price"),
        yaxis = list(title = "DTM"),
        zaxis = list(title ="IV")
      )) %>%  add_surface(x=grid$X, y=grid$Y)
  })
  
  
  # Strategy Builder tab
  output$strategy <- renderUI({
    
  })
  
}

shinyApp(ui, server)
