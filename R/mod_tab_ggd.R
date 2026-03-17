library(shiny)

source("R/components/mod_trend.R")
source("R/components/mod_micro.R")
source("R/components/mod_regio_map.R")
source("R/components/mod_kpi.R")

mod_tab_ggd_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      class = "amr-top-row",
      mod_trend_ui(ns("trend")),
      mod_kpi_ui(ns("kpi"))
    ),
    
    fluidRow(
      mod_micro_ui(ns("micro")),
      mod_regio_map_ui(ns("map"))
    ),
    
    tags$div(
      class = "amr-footer",
      tags$div(
        class = "left",
        "© AMR Zorgnetwerk Noord-Nederland, 2024. Laatste update: april 2024. BRMO = bijzonder resistente micro-organismen."
      ),
      tags$div(
        class = "right",
        "Meldplichtig: ESBL, MRSA, VRE, CPE."
      )
    )
  )
}

mod_tab_ggd_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    
    mod_trend_server("trend", data, cfg)
    mod_kpi_server("kpi", data, cfg)
    mod_micro_server("micro", data, cfg)
    mod_regio_map_server("map", data, cfg)
    
  })
}