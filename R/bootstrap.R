# ---------------------------------------------------------
# Bootstrap: package management
# ---------------------------------------------------------
# Alle packages worden hier eenmalig geladen.
# Individuele R-bestanden roepen GEEN library() aan.
#
# Voor reproduceerbare omgevingen: gebruik {renv}.
#   renv::init()   — initialiseer lockfile
#   renv::restore() — herstel exacte versies op andere machine
# ---------------------------------------------------------

required_packages <- c(
  "here",
  "shiny",
  "shinydashboard",
  "leaflet",
  "plotly",
  "dplyr",
  "tidyr",
  "readr",
  "sf",
  "yaml",
  "htmlwidgets",
  "ggiraph",
  "ggplot2"
)

.bootstrap_packages <- function(packages) {
  missing_pkgs <- packages[!packages %in% rownames(installed.packages())]

  if (length(missing_pkgs) > 0) {
    message("Ontbrekende packages worden geïnstalleerd: ",
            paste(missing_pkgs, collapse = ", "))
    install.packages(missing_pkgs, dependencies = TRUE)
  }

  invisible(lapply(packages, function(pkg) {
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE)
    )
  }))
}

.bootstrap_packages(required_packages)