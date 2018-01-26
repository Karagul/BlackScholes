# Title     : Shiny App Pricing Tab
# Objective : Compartmentalize the app in order to make development of individual parts easier.
# Created by: Tyler
# Created on: 2018-01-13
library(shiny)
library(tidyverse)
library(pracma)
library(plotly)
library(shinycssloaders)




ui <- fluidPage(

  titlePanel("BlackScholes Options Dashboard"),

    mainPanel(
      tabsetPanel(
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
          
          mainPanel(withSpinner(plotlyOutput("volsurface", width="150%", height="175%")))),

                 tableOutput("pricing"))

      )
    )


)


server <- function(input, output, session) {

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
}



shinyApp(ui, server)
