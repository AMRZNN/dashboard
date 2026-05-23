library(here)

source(here("R", "data_service.R"))

source(here("R", "mod_tab_ggd.R"))
source(here("R", "mod_tab_ziekenhuizen.R"))
source(here("R", "mod_tab_laboratoria.R"))
source(here("R", "mod_tab_huisartsen.R"))
source(here("R", "mod_tab_verpleeghuizen.R"))

app_server <- function(input, output, session, cfg) {
  
  data     <- data_service(cfg)
  weergave <- reactive({ if (is.null(input$weergave)) "absoluut" else input$weergave })
  
  mod_tab_ggd_server("ggd",         data, cfg, weergave)
  mod_tab_ziekenhuizen_server("zh", data, cfg)
  mod_tab_laboratoria_server("lab", data, cfg, weergave)
  mod_tab_huisartsen_server("ha",   data, cfg)
  mod_tab_verpleeghuizen_server("vh", data, cfg)
}