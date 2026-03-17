library(shiny)

mod_tab_huisartsen_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 12,
        title = "Huisartsen dashboard",
        h4("AMR signalen uit huisartsenpraktijken"),
        p("Hier komt specifieke content voor huisartsen.")
      )
    )
  )
}

mod_tab_huisartsen_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    # toekomstige logica
  })
}