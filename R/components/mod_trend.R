library(shiny)
library(plotly)
library(leaflet)
library(dplyr)

mod_trend_ui <- function(id) {
  ns <- NS(id)
  
  box(
    width = 8,
    class = "amr-fixed-box trend-box",
    title = "Incidentie van BRMO meldingen",
    
    tags$div(
      class = "amr-subtitle",
      "Aantal meldingen per 100.000 inwoners, Noord-Nederland, 2015–2024"
    ),
    
    tags$div(
      class = "trend-absolute-wrapper",
      
      tags$div(
        class = "trend-plot-area",
        plotlyOutput(ns("plot"), height = "100%")
      ),
      
      tags$div(
        class = "trend-map-area",
        
        leafletOutput(ns("mini_map"), height = "100%"),
        
        tags$div(
          id = ns("map_year"),
          class = "mini-map-year"
        )
      )
    )
  )
}