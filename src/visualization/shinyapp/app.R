library(shiny)
library(tidyverse)



point <- function(x, y, group){
  # Point object for performing K-Means Clustering
  #
  # Args:
  #   x (numeric): Rectangular data to be clustered 
  #   y (numeric): Number of clusters
  #   group (numeric): Number of replications to perform.
  # Returns:
  #   obj (list): List containing x and y coordinates, and the group of the point.
  #
  obj <- list(x=x,y=y,group=group)
  
  class(obj) <- append(class(obj), "point")
  return(obj)
}


greekSurface <- function(strike=1, vol=0.15, rfr=0.05,
                         maturity=2, money_range=c(0.5, 1.5)) {

  # Surface object for 3d plots of option greeks. Takes in arguments for the parameters of the plot.
  #
  #
  # Args:
  #   strike (numeric):
  #   vol (numeric):
  #   rfr (numeric):
  #   maturity (numeric):
  #   money_range (numeric):
  #
  # Returns:
  #   obj (list(X=grid$X, Y=grid$Y, strike=strike, vol=vol, rfr=rfr,
  #        maturity_range=maturity_range, money_range=money_range):
  #
  #             X is the X component of the meshgrid, Y is the Y component. Capitals denote meshgrid and not original variables.
  #             strike is if using actual stock data rather than a generic surface. vol and rfr are parameters for the greeks.
  #             money_range refers to moneyness, and the ranges are for the axes limits.
  #
  #
  #
  #

  moneyness = seq(from=strike*money_range[0],
               to=strike*money_range[1],by=(strike*money_range[1] - strike*money_range[0])/100)

  grid <- meshgrid(moneyness, maturities)

  obj <- list(X=grid$X, Y=grid$Y, strike=strike, vol=vol, rfr=rfr,
              maturity_range=maturity_range, money_range=money_range)

   # Set the name of the class returned by this class function
  class(obj) <- append(class(obj), "greekSurface")

 return (obj)
}

calc_d <- function(grid, strike, vol, rfr){
  # Calculates the d1 portion of the Black Scholes formula
  out <- (1/(vol*sqrt(grid$Y)))*(log(grid$X)/strike) + ((rfr + ((vol^2))*grid$Y))
}


plot_surface <- function()





bcl <- read.csv('bcl-data.csv', stringsAsFactors=FALSE)
print(head(bcl))

ui <- fluidPage(
  
  titlePanel("BlackScholes Options Dashboard"),
  sidebarLayout(
    sidebarPanel(
      textInput("rfr", label = "'Risk-free' Rate", value = 0.05)
      textInput("vol", label = "Volatility", value = 0.13)

      uiOutput("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),

      selectInput("countryInput", "Country",
                  choices = c("CANADA", "FRANCE", "ITALY"))),
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      tableOutput("results")
    )
  )
  
  
)

server <- function(input, output, session) {
  output$coolplot <- renderPlot({
    filtered <- bcl %>% 
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput)
    
    ggplot(filtered, aes(Alcohol_Content)) +
      geom_histogram()
  })  
  
  output$results <- renderTable({
    filtered <- bcl %>% 
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput)
    filtered
  })
  
}

shinyApp(ui, server)
