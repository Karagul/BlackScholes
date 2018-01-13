# Title     : Shiny App's Greek Surface Tab
# Objective : Compartmentalize the app in order to make development of individual parts easier.
# Created by: Tyler
# Created on: 2018-01-13
library(shiny)
library(tidyverse)
library(pracma)
library(plotly)




ui <- fluidPage(

  titlePanel("BlackScholes Options Dashboard"),

    mainPanel(
      tabsetPanel(

        tabPanel("Greek Surface",
                         sidebarLayout(
                           sidebarPanel(
                             textInput("strike", label = "Strike Price", value = 1),
                             textInput("rfr", label = "'Risk-free' Rate", value = 0.05),
                             textInput("vol", label = "Volatility", value = 0.13),
                             sliderInput('maturity', "Maturity",
                                         min=1, max=1095, value = 90,
                                         sep="", step=15),
                             sliderInput('money_range', "Moneyness",
                                         min=0, max=2, value = c(0.5, 1.5),
                                         sep="", step=0.1),
                             # This input will need to be moved to a renderUI element conditional on "greekOrder"
                             selectInput("greek", "Greek",
                                         choices=c('Delta','Vega', 'Theta', 'Rho', 'Psi'),
                                         selected='Delta'),
                             radioButtons('greekOrder', "Order",
                                          choices=c("First","Second","Third"),
                                          selected="First"),
                             radioButtons('contract', label="Contract Type",
                                          choices=c("Call","Put"),
                                          selected='Call')
                           ),
                         mainPanel(plotlyOutput("greekplot", width="150%", height="175%")))
                          ),
      )))



  server <- function(input, output, session) {
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
                  zaxis = list(title = glue({capitalize(greek)}))
                )) %>%  add_surface(x=grid$X, y=grid$Y)
          })
  }


shinyApp(ui, server)
