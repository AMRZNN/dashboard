# ---------------------------------------------------------
# Bootstrap: package management
# ---------------------------------------------------------

required_packages <- c(
  "shiny",
  "shinydashboard",
  "plotly",
  "leaflet",
  "dplyr",
  "tidyr",
  "readr",
  "sf",
  "yaml",
  "htmlwidgets"
)

install_if_missing <- function(packages) {
  
  installed <- installed.packages()[, "Package"]
  
  for (pkg in packages) {
    
    if (!pkg %in% installed) {
      message("Installing missing package: ", pkg)
      install.packages(pkg, dependencies = TRUE)
    }
    
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE)
    )
  }
}

install_if_missing(required_packages)