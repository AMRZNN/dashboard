source("R/bootstrap.R")

source("R/config_loader.R")
source("R/app_ui.R")
source("R/app_server.R")

cfg <- load_config()

shinyApp(
  ui = app_ui(cfg),
  server = function(input, output, session) {
    app_server(input, output, session, cfg)
  }
)