library(shiny)
library(plotly)
library(dplyr)

mod_micro_ui <- function(id) {
  ns <- NS(id)
  
  box(
    width = 8,
    title = "Belangrijkste BRMO micro-organismen",
    tags$div(class="amr-subtitle",
             "Verdeeld naar micro-organismen, Noord-Nederland"),
    plotlyOutput(ns("plot"), height="290px")
  )
}

mod_micro_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    
    output$plot <- renderPlotly({
      
      df <- data$micro()
      
      plot_ly(
        df,
        x = ~jaar,
        y = ~waarde,
        color = ~type,
        type = "bar"
      ) %>%
        layout(
          barmode = "stack",
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)"
        )
    })
    
  })
}