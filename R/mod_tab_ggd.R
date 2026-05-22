library(here)

source(here("R", "components", "mod_trend.R"))
source(here("R", "components", "mod_micro.R"))
source(here("R", "components", "mod_regio_map.R"))
source(here("R", "components", "mod_kpi.R"))

# =========================
# UI
# =========================
mod_tab_ggd_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      class = "amr-row1",
      mod_trend_ui(ns("trend")),
      mod_kpi_ui(ns("kpi"))
    ),
    
    fluidRow(
      class = "amr-row2",
      mod_micro_ui(ns("micro")),
      mod_regio_map_ui(ns("map"))
    ),
    
    tags$div(
      class = "amr-footer",
      tags$div(class = "left",
               "© AMR Zorgnetwerk Noord-Nederland, 2024. Bron: Certe laboratorium. BRMO = bijzonder resistente micro-organismen. ",
               tags$a(href = "https://github.com/AMRZNN/dashboard_data/blob/main/TERMS_OF_USE.md",
                      target = "_blank", "Gebruiksvoorwaarden")),
      tags$div(class = "right",
               "Meldplichtig: ESBL, MRSA, VRE, CPE.")
    )
  )
}

# =========================
# SERVER
# =========================
mod_tab_ggd_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    
    noord_nuts3 <- c(
      "Delfzijl en omgeving", "Oost-Groningen", "Overig Groningen",
      "Noord-Friesland", "Zuidoost-Friesland", "Zuidwest-Friesland",
      "Noord-Drenthe", "Zuidoost-Drenthe", "Zuidwest-Drenthe"
    )
    
    # Aggregeer regio-data naar nuts3-niveau, uitgedrukt per 100.000 inwoners
    # Inwonersaantallen komen uit de shape (geo_nuts3.rds)
    ggd_regio <- reactive({
      df  <- data$regio()
      shp <- data$shape
      req(!is.null(df))
      
      # Haal inwoners uit shape
      inwoners_df <- sf::st_drop_geometry(shp) |>
        dplyr::select(nuts3, inwoners)
      
      df |>
        dplyr::mutate(jaar  = as.integer(jaar),
                      maand = as.integer(maand),
                      datum = as.Date(paste(jaar, maand, "01", sep = "-"))) |>
        dplyr::filter(datum == max(datum, na.rm = TRUE)) |>
        dplyr::mutate(dplyr::across(c(esbl, mrsa, vre, cpe, mrpa,
                                      facre, cre, fara, cpa, ca), as.numeric)) |>
        dplyr::mutate(totaal = rowSums(
          dplyr::across(c(esbl, mrsa, vre, cpe, mrpa,
                          facre, cre, fara, cpa, ca)), na.rm = TRUE)) |>
        dplyr::group_by(regio = nuts3) |>
        dplyr::summarise(meldingen = sum(totaal, na.rm = TRUE), .groups = "drop") |>
        dplyr::left_join(inwoners_df, by = c("regio" = "nuts3")) |>
        dplyr::mutate(incidentie = round(meldingen / inwoners * 100000, 1))
    })
    
    # Trendgrafiek: laatste 12 maanden per 100.000 inwoners
    ggd_trend <- reactive({
      df  <- data$regio()
      shp <- data$shape
      req(!is.null(df))
      
      # Totaal inwoners Noord-NL uit shape
      inwoners_totaal <- sum(
        sf::st_drop_geometry(shp) |>
          dplyr::filter(nuts3 %in% noord_nuts3) |>
          dplyr::pull(inwoners),
        na.rm = TRUE
      )
      
      df |>
        dplyr::mutate(dplyr::across(c(esbl, mrsa, vre, cpe, mrpa,
                                      facre, cre, fara, cpa, ca), as.numeric),
                      jaar  = as.integer(jaar),
                      maand = as.integer(maand),
                      datum = as.Date(paste(jaar, maand, "01", sep = "-"))) |>
        dplyr::mutate(totaal = rowSums(
          dplyr::across(c(esbl, mrsa, vre, cpe, mrpa,
                          facre, cre, fara, cpa, ca)), na.rm = TRUE)) |>
        dplyr::group_by(datum, jaar, maand) |>
        dplyr::summarise(meldingen = sum(totaal, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(incidentie = round(meldingen / inwoners_totaal * 100000, 1)) |>
        dplyr::arrange(datum) |>
        (\(d) { cutoff <- seq(max(d$datum), length.out = 2, by = "-11 months")[2]
        dplyr::filter(d, datum >= cutoff) })()
    })
    
    # KPI's: per categorie per 100.000 inwoners, laatste 12 maanden
    ggd_kpi <- reactive({
      df  <- data$regio()
      shp <- data$shape
      req(!is.null(df))
      
      inwoners_totaal <- sum(
        sf::st_drop_geometry(shp) |>
          dplyr::filter(nuts3 %in% noord_nuts3) |>
          dplyr::pull(inwoners),
        na.rm = TRUE
      )
      
      df |>
        dplyr::mutate(dplyr::across(c(esbl, mrsa, vre, cpe), as.numeric),
                      jaar  = as.integer(jaar),
                      maand = as.integer(maand),
                      datum = as.Date(paste(jaar, maand, "01", sep = "-"))) |>
        dplyr::group_by(datum) |>
        dplyr::summarise(
          ESBL = sum(esbl, na.rm = TRUE),
          MRSA = sum(mrsa, na.rm = TRUE),
          VRE  = sum(vre,  na.rm = TRUE),
          CPE  = sum(cpe,  na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          ESBL = round(ESBL / inwoners_totaal * 100000, 2),
          MRSA = round(MRSA / inwoners_totaal * 100000, 2),
          VRE  = round(VRE  / inwoners_totaal * 100000, 2),
          CPE  = round(CPE  / inwoners_totaal * 100000, 2)
        ) |>
        dplyr::arrange(datum)
    })
    
    ggd_data <- list(
      trend = ggd_trend,
      micro = data$micro,
      regio = ggd_regio,
      shape = data$shape,
      kpi   = ggd_kpi
    )
    
    mod_trend_server("trend",   ggd_data, cfg)
    mod_kpi_server("kpi",       ggd_data, cfg)
    mod_micro_server("micro",   ggd_data, cfg)
    mod_regio_map_server("map", ggd_data, cfg)
  })
}
