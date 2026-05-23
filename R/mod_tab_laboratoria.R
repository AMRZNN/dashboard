library(here)

# =========================
# UI
# =========================
mod_tab_laboratoria_ui <- function(id) {
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
mod_tab_laboratoria_server <- function(id, data, cfg, weergave = reactive({ "absoluut" })) {
  moduleServer(id, function(input, output, session) {
    
    noord <- cfg$geo$noord_provincies
    
    # Helper: laatste 12 maanden filteren
    laatste_12 <- function(df) {
      df <- df |>
        dplyr::mutate(
          jaar  = as.integer(jaar),
          maand = as.integer(maand),
          datum = as.Date(paste(jaar, maand, "01", sep = "-"))
        )
      cutoff <- seq(max(df$datum, na.rm = TRUE), length.out = 2, by = "-11 months")[2]
      dplyr::filter(df, datum >= cutoff)
    }
    
    # --- Trendgrafiek: som per maand, laatste 12 maanden ---
    lab_trend <- reactive({
      df <- data$certe()
      req(!is.null(df))
      
      df |>
        dplyr::filter(provincie %in% noord) |>
        dplyr::mutate(dplyr::across(c(esbl, mrsa, vre, cpe, mrpa,
                                      facre, cre, fara, cpa, ca), as.numeric)) |>
        dplyr::mutate(totaal = rowSums(dplyr::across(c(esbl, mrsa, vre, cpe,
                                                       mrpa, facre, cre, fara,
                                                       cpa, ca)), na.rm = TRUE),
                      jaar  = as.integer(jaar),
                      maand = as.integer(maand),
                      datum = as.Date(paste(jaar, maand, "01", sep = "-"))) |>
        dplyr::group_by(datum, jaar, maand) |>
        dplyr::summarise(meldingen = sum(totaal, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(datum) |>
        (\(d) { cutoff <- seq(max(d$datum), length.out = 2, by = "-11 months")[2]; dplyr::filter(d, datum >= cutoff) })() |>
        dplyr::mutate(incidentie = {
          w <- if (is.function(weergave) || is.reactive(weergave)) weergave() else weergave
          if (w == "per100k") {
            inwoners_totaal <- sum(
              sf::st_drop_geometry(data$shape) |>
                dplyr::filter(nuts3 %in% c(
                  "Delfzijl en omgeving", "Oost-Groningen", "Overig Groningen",
                  "Noord-Friesland", "Zuidoost-Friesland", "Zuidwest-Friesland",
                  "Noord-Drenthe", "Zuidoost-Drenthe", "Zuidwest-Drenthe"
                )) |>
                dplyr::pull(inwoners), na.rm = TRUE
            )
            round(meldingen / inwoners_totaal * 100000, 1)
          } else meldingen
        })
    })
    
    # --- KPI's: per categorie, vergelijking t.o.v. vorige maand ---
    lab_kpi <- reactive({
      df <- data$certe()
      req(!is.null(df))
      
      df |>
        dplyr::filter(provincie %in% noord) |>
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
        dplyr::arrange(datum) |>
        (\(d) {
          w <- if (is.function(weergave) || is.reactive(weergave)) weergave() else weergave
          if (w == "per100k") {
            inwoners_totaal <- sum(
              sf::st_drop_geometry(data$shape) |>
                dplyr::filter(nuts3 %in% c(
                  "Delfzijl en omgeving", "Oost-Groningen", "Overig Groningen",
                  "Noord-Friesland", "Zuidoost-Friesland", "Zuidwest-Friesland",
                  "Noord-Drenthe", "Zuidoost-Drenthe", "Zuidwest-Drenthe"
                )) |>
                dplyr::pull(inwoners), na.rm = TRUE
            )
            dplyr::mutate(d,
                          ESBL = round(ESBL / inwoners_totaal * 100000, 2),
                          MRSA = round(MRSA / inwoners_totaal * 100000, 2),
                          VRE  = round(VRE  / inwoners_totaal * 100000, 2),
                          CPE  = round(CPE  / inwoners_totaal * 100000, 2)
            )
          } else d
        })()
    })
    
    # --- Micro: uitsplitsing per jaar ---
    lab_micro <- reactive({
      df <- data$certe()
      req(!is.null(df))
      
      df |>
        dplyr::filter(provincie %in% noord) |>
        dplyr::mutate(dplyr::across(c(esbl, mrsa, vre, cpe,
                                      mrpa, facre, cre, fara, cpa, ca), as.numeric)) |>
        dplyr::group_by(jaar = as.integer(jaar)) |>
        dplyr::summarise(
          ESBL   = sum(esbl,  na.rm = TRUE),
          MRSA   = sum(mrsa,  na.rm = TRUE),
          VRE    = sum(vre,   na.rm = TRUE),
          CPE    = sum(cpe,   na.rm = TRUE),
          Overig = sum(mrpa + facre + cre + fara + cpa + ca, na.rm = TRUE),
          .groups = "drop"
        ) |>
        tidyr::pivot_longer(cols = c(ESBL, MRSA, VRE, CPE, Overig),
                            names_to = "type", values_to = "waarde") |>
        dplyr::arrange(jaar)
    })
    
    # Regio-aggregatie uit Certe: totaal BRMO per provincie, laatste maand
    certe_regio <- reactive({
      df <- data$certe()
      req(!is.null(df))
      
      df |>
        dplyr::mutate(
          jaar  = as.integer(jaar),
          maand = as.integer(maand),
          datum = as.Date(paste(jaar, maand, "01", sep = "-")),
          dplyr::across(c(esbl, mrsa, vre, cpe, mrpa,
                          facre, cre, fara, cpa, ca), as.numeric)
        ) |>
        dplyr::mutate(totaal = rowSums(
          dplyr::across(c(esbl, mrsa, vre, cpe, mrpa,
                          facre, cre, fara, cpa, ca)), na.rm = TRUE)) |>
        dplyr::filter(datum == max(datum, na.rm = TRUE)) |>
        dplyr::group_by(regio = nuts3) |>
        dplyr::summarise(meldingen = sum(totaal, na.rm = TRUE), .groups = "drop") |>
        dplyr::left_join(
          sf::st_drop_geometry(data$shape) |> dplyr::select(nuts3, inwoners),
          by = c("regio" = "nuts3")
        ) |>
        dplyr::mutate(incidentie = if (weergave() == "per100k")
          round(meldingen / inwoners * 100000, 1)
          else meldingen)
    })
    
    # Samengesteld data-object
    lab_data <- list(
      trend       = lab_trend,
      micro       = lab_micro,
      regio       = certe_regio,   # Certe regio-data
      shape       = data$shape,    # zelfde shape
      regio_basis = data$regio,    # GGD regio als fallback voor minimap
      kpi         = lab_kpi
    )
    
    mod_trend_server("trend",   lab_data, cfg, eenheid = weergave)
    mod_kpi_server("kpi",       lab_data, cfg)
    mod_micro_server("micro",   lab_data, cfg)
    mod_regio_map_server("map", lab_data, cfg, weergave)
  })
}