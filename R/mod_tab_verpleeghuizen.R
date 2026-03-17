library(shiny)

mod_tab_verpleeghuizen_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 12,
        title = "Verpleeghuizen dashboard",
        h4("AMR monitoring verpleeghuizen"),
        p("Hier komt specifieke content voor verpleeghuizen.")
      )
    )
  )
}

mod_tab_verpleeghuizen_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    # toekomstige logica
  })
}