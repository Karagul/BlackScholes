# Title     : TODO
# Objective : TODO
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

        tabPanel("Strategy Planner", uiOutput('strategy'))
      )
    )


)


server <- function(input, output, session) {
  # Strategy Builder tab
  output$strategy <- renderUI({

  })

}


shinyApp(ui, server)










