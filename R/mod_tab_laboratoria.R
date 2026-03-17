library(shiny)

mod_tab_laboratoria_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 12,
        title = "Laboratoria dashboard",
        h4("Laboratorium meldingen en trends"),
        p("Hier komt specifieke content voor laboratoria.")
      )
    )
  )
}

mod_tab_laboratoria_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    # toekomstige logica
  })
}