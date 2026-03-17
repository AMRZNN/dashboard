library(shiny)

mod_tab_ziekenhuizen_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Ziekenhuizen dashboard"),
    p("Placeholder voor ziekenhuizen")
  )
}

mod_tab_ziekenhuizen_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    # later uitbreiden
  })
}