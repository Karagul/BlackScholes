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
  #   out (numeric): Black Scholes option premium.
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





ui <- fluidPage(
  
  titlePanel("BlackScholes Options Dashboard"),
  sidebarLayout(
    sidebarPanel(
      textInput("strike", label = "Strike Price", value = 100),
      textInput("rfr", label = "'Risk-free' Rate", value = 0.05),
      textInput("vol", label = "Volatility", value = 0.13),
      selectInput("greek", "Greek", 
                  choices=c('delta',''),
                  selected='delta')
      ),

    mainPanel(
      tabsetPanel(

        tabPanel("Plot", plotlyOutput("mainplot")),
        tabPanel("Summary", tableOutput("summary")),
        tabPanel("Table", tableOutput("table"))
        tabPanel("Greek Surface", plotlyOutput("mainplot")),
        tabPanel("P/L Chart", tableOutput("plchart")),
        tabPanel("Pricing", sidebarPanel),
        tabPanel("Strategy Planner", uiOutput('strategy'))

      )
    )
  
  
))

server <- function(input, output, session) {
  output$mainplot <- renderPlotly({
    
    strike=1
    vol=0.15
    rfr=0.05
    maturity=0.2
    money_range=c(0.5, 1.5)
    
    calc_d <- function(grid, strike, vol, rfr){
      # Calculates the d1 portion of the Black Scholes formula
      out <- (1/(vol*sqrt(grid$Y)))*(log(grid$X)/strike) + ((rfr + ((vol^2))*grid$Y))
    }
      
      greek <- 'delta'
       ## I Think that adding the dymanic input messed up the function (reading in as string?)
      moneyness <-  seq(from=input$strike*money_range[1],
                        to=input$strike*money_range[2],by=(input$strike*money_range[2] - input$strike*money_range[1])/100)
      
      maturities <- seq(from=1, to=maturity*365 ,by=1)
      
      grid <- meshgrid(x=moneyness, y=maturities)
      

      d1 <- calc_d(grid, input$strike, input$vol, input$rfr)
      d2 <- d1 - vol*sqrt(grid$Y)
      

      Z <- pnorm(d1)
      

      add_surface(plot_ly(z = ~Z))

  })  
  
 # output$summary <- renderTable({
    
  # })
  
  output$table <- renderTable({
    filtered <- bcl %>% 
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput)
    filtered
  })
  
}

shinyApp(ui, server)
