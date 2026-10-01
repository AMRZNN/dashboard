# ---------------------------------------------------------
# Bootstrap: package management
# ---------------------------------------------------------
# Alle packages worden hier eenmalig geladen.
# Individuele R-bestanden roepen GEEN library() aan.
#
# Package-versies worden beheerd met {renv} (zie renv.lock).
#   renv::restore()  — installeer exacte versies uit renv.lock
#   renv::snapshot() — leg gewijzigde/nieuwe packages vast in renv.lock
#
# Gebruik hier expliciete library()-aanroepen (geen vector met
# character.only = TRUE): alleen zo detecteert renv de packages.
# ---------------------------------------------------------

suppressPackageStartupMessages({
  library(here)
  library(shiny)
  library(shinydashboard)
  library(leaflet)
  library(plotly)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(sf)
  library(yaml)
  library(htmlwidgets)
  library(ggplot2)
})
