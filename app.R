if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)

source(here("R", "bootstrap.R"))
source(here("R", "config_loader.R"))
source(here("R", "app_ui.R"))
source(here("R", "app_server.R"))

cfg <- load_config()

shinyApp(
  ui = app_ui(cfg),
  server = function(input, output, session) {
    app_server(input, output, session, cfg)
  }
)
