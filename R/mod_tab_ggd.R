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
    )
  )
}

# =========================
# SERVER
# =========================
mod_tab_ggd_server <- function(id, data, cfg,
                               weergave = reactive({ "absoluut" }),
                               dataset  = reactive({ "brmo" }),
                               pathogenen = reactive({ c("esbl","mrsa","vre","cpe") })) {
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
        dplyr::mutate(incidentie = if (weergave() == "per100k")
          round(meldingen / inwoners * 100000, 1)
          else meldingen)
    })
    
    # Trendgrafiek: laatste 12 maanden, relatief of absoluut
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
        dplyr::mutate(incidentie = if (weergave() == "per100k")
          round(meldingen / inwoners_totaal * 100000, 1)
          else meldingen) |>
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
          ESBL = if (weergave() == "per100k") round(ESBL / inwoners_totaal * 100000, 2) else ESBL,
          MRSA = if (weergave() == "per100k") round(MRSA / inwoners_totaal * 100000, 2) else MRSA,
          VRE  = if (weergave() == "per100k") round(VRE  / inwoners_totaal * 100000, 2) else VRE,
          CPE  = if (weergave() == "per100k") round(CPE  / inwoners_totaal * 100000, 2) else CPE
        ) |>
        dplyr::arrange(datum)
    })
    
    # Respiratoir-reactives voor GGD
    resp_kolommen <- reactive({
      sel <- pathogenen()
      alle_v <- cfg$respiratoir$alle_virussen
      kolommen <- intersect(sel, alle_v)
      if (length(kolommen) == 0) alle_v else kolommen
    })
    
    inwoners_totaal_r <- reactive({
      sum(sf::st_drop_geometry(data$shape) |>
            dplyr::filter(nuts3 %in% noord_nuts3) |>
            dplyr::pull(inwoners), na.rm = TRUE)
    })
    
    resp_trend_ggd <- reactive({
      df <- data$respiratoir(); req(!is.null(df))
      kolommen <- resp_kolommen()
      df |>
        dplyr::filter(nuts3 %in% noord_nuts3) |>
        dplyr::mutate(jaar = as.integer(jaar), maand = as.integer(maand),
                      datum = as.Date(paste(jaar, maand, "01", sep = "-")),
                      dplyr::across(dplyr::all_of(kolommen), as.numeric)) |>
        dplyr::mutate(totaal = rowSums(dplyr::across(dplyr::all_of(kolommen)), na.rm = TRUE)) |>
        dplyr::group_by(datum, jaar, maand) |>
        dplyr::summarise(meldingen = sum(totaal, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(datum) |>
        (\(d) { cutoff <- seq(max(d$datum), length.out = 2, by = "-11 months")[2]
        dplyr::filter(d, datum >= cutoff) })() |>
        dplyr::mutate(incidentie = if (weergave() == "per100k")
          round(meldingen / inwoners_totaal_r() * 100000, 1) else meldingen)
    })
    
    resp_kpi_ggd <- reactive({
      df <- data$respiratoir(); req(!is.null(df))
      kpi_v <- cfg$respiratoir$kpi_virussen
      inw   <- inwoners_totaal_r()
      df |>
        dplyr::filter(nuts3 %in% noord_nuts3) |>
        dplyr::mutate(jaar = as.integer(jaar), maand = as.integer(maand),
                      datum = as.Date(paste(jaar, maand, "01", sep = "-")),
                      dplyr::across(dplyr::all_of(kpi_v), as.numeric)) |>
        dplyr::group_by(datum) |>
        dplyr::summarise(dplyr::across(dplyr::all_of(kpi_v), ~ sum(.x, na.rm = TRUE)),
                         .groups = "drop") |>
        dplyr::arrange(datum) |>
        (\(d) if (weergave() == "per100k")
          dplyr::mutate(d, dplyr::across(dplyr::all_of(kpi_v), ~ round(.x / inw * 100000, 2)))
         else d)()
    })
    
    resp_micro_ggd <- reactive({
      df <- data$respiratoir(); req(!is.null(df))
      alle_v <- cfg$respiratoir$alle_virussen
      df |>
        dplyr::filter(nuts3 %in% noord_nuts3) |>
        dplyr::mutate(jaar = as.integer(jaar),
                      dplyr::across(dplyr::all_of(alle_v), as.numeric)) |>
        dplyr::group_by(jaar) |>
        dplyr::summarise(dplyr::across(dplyr::all_of(alle_v), ~ sum(.x, na.rm = TRUE)),
                         .groups = "drop") |>
        tidyr::pivot_longer(cols = dplyr::all_of(alle_v),
                            names_to = "type", values_to = "waarde") |>
        dplyr::arrange(jaar)
    })
    
    resp_regio_ggd <- reactive({
      df <- data$respiratoir(); req(!is.null(df))
      kolommen <- resp_kolommen()
      df |>
        dplyr::mutate(jaar = as.integer(jaar), maand = as.integer(maand),
                      datum = as.Date(paste(jaar, maand, "01", sep = "-")),
                      dplyr::across(dplyr::all_of(kolommen), as.numeric)) |>
        dplyr::mutate(totaal = rowSums(dplyr::across(dplyr::all_of(kolommen)), na.rm = TRUE)) |>
        dplyr::filter(datum == max(datum, na.rm = TRUE)) |>
        dplyr::group_by(regio = nuts3) |>
        dplyr::summarise(meldingen = sum(totaal, na.rm = TRUE), .groups = "drop") |>
        dplyr::left_join(sf::st_drop_geometry(data$shape) |> dplyr::select(nuts3, inwoners),
                         by = c("regio" = "nuts3")) |>
        dplyr::mutate(incidentie = if (weergave() == "per100k")
          round(meldingen / inwoners * 100000, 1) else meldingen)
    })
    
    # Dataset-aware data-object
    ggd_data <- reactive({
      if (dataset() == "respiratoir") {
        list(trend = resp_trend_ggd, micro = resp_micro_ggd,
             regio = resp_regio_ggd, shape = data$shape, kpi = resp_kpi_ggd)
      } else {
        list(trend = ggd_trend, micro = data$micro,
             regio = ggd_regio, shape = data$shape, kpi = ggd_kpi)
      }
    })
    
    mod_trend_server("trend",   ggd_data, cfg, eenheid = weergave, dataset = dataset)
    mod_kpi_server("kpi",       ggd_data, cfg, dataset = dataset)
    mod_micro_server("micro",   ggd_data, cfg, dataset = dataset)
    mod_regio_map_server("map", ggd_data, cfg, weergave, dataset = dataset)
  })
}