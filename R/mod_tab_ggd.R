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
                               weergave          = reactive({ "absoluut" }),
                               dataset           = reactive({ "brmo" }),
                               pathogenen        = reactive({ c("esbl","mrsa","vre","cpe") }),
                               alle_geselecteerd = reactive({ FALSE })) {
  moduleServer(id, function(input, output, session) {
    
    .BRMO_COLS   <- c("esbl","mrsa","vre","cpe","mrpa","facre","cre","fara","cpa","ca")
    .BRMO_LABELS <- c(esbl="ESBL", mrsa="MRSA", vre="VRE", cpe="CPE",
                      mrpa="MRPA", facre="FACRE", cre="CRE", fara="FARA", cpa="CPA", ca="CA")
    
    noord_nuts3 <- c(
      "Delfzijl en omgeving", "Oost-Groningen", "Overig Groningen",
      "Noord-Friesland", "Zuidoost-Friesland", "Zuidwest-Friesland",
      "Noord-Drenthe", "Zuidoost-Drenthe", "Zuidwest-Drenthe"
    )
    
    # Actieve BRMO-selectie
    brmo_sel <- reactive({
      intersect(pathogenen(), .BRMO_COLS)   # character(0) bij lege selectie
    })
    
    # Helper: inwoners totaal Noord-Nederland
    get_inwoners <- function(shp) {
      sum(sf::st_drop_geometry(shp) |>
            dplyr::filter(nuts3 %in% noord_nuts3) |>
            dplyr::pull(inwoners), na.rm = TRUE)
    }
    
    # Regio-kaart — gefilterd op geselecteerde pathogenen
    ggd_regio <- reactive({
      df  <- data$regio()
      shp <- data$shape
      req(!is.null(df))
      sel <- brmo_sel()
      
      df |>
        dplyr::mutate(jaar  = as.integer(jaar),
                      maand = as.integer(maand),
                      datum = as.Date(paste(jaar, maand, "01", sep = "-"))) |>
        dplyr::filter(datum == max(datum, na.rm = TRUE)) |>
        dplyr::mutate(dplyr::across(dplyr::any_of(sel), as.numeric)) |>
        dplyr::mutate(totaal = rowSums(dplyr::across(dplyr::any_of(sel)), na.rm = TRUE)) |>
        dplyr::group_by(regio = nuts3) |>
        dplyr::summarise(meldingen = sum(totaal, na.rm = TRUE), .groups = "drop") |>
        dplyr::left_join(sf::st_drop_geometry(shp) |> dplyr::select(nuts3, inwoners),
                         by = c("regio" = "nuts3")) |>
        dplyr::mutate(incidentie = if (weergave() == "per100k")
          round(meldingen / inwoners * 100000, 1) else meldingen)
    })
    
    # Trendgrafiek — gefilterd op geselecteerde pathogenen
    ggd_trend <- reactive({
      df  <- data$regio()
      shp <- data$shape
      req(!is.null(df))
      sel <- brmo_sel()
      inwoners_totaal <- get_inwoners(shp)
      
      df |>
        dplyr::mutate(dplyr::across(dplyr::any_of(sel), as.numeric),
                      jaar  = as.integer(jaar),
                      maand = as.integer(maand),
                      datum = as.Date(paste(jaar, maand, "01", sep = "-"))) |>
        dplyr::mutate(totaal = rowSums(dplyr::across(dplyr::any_of(sel)), na.rm = TRUE)) |>
        dplyr::group_by(datum, jaar, maand) |>
        dplyr::summarise(meldingen = sum(totaal, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(incidentie = if (weergave() == "per100k")
          round(meldingen / inwoners_totaal * 100000, 1) else meldingen) |>
        dplyr::arrange(datum) |>
        (\(d) { cutoff <- seq(max(d$datum), length.out = 2, by = "-11 months")[2]
        dplyr::filter(d, datum >= cutoff) })()
    })
    
    # KPI's — gefilterd: toon de 4 grootste van de selectie
    ggd_kpi <- reactive({
      df  <- data$regio()
      shp <- data$shape
      req(!is.null(df))
      sel <- brmo_sel()
      inwoners_totaal <- get_inwoners(shp)
      
      df |>
        dplyr::mutate(dplyr::across(dplyr::any_of(sel), as.numeric),
                      jaar  = as.integer(jaar),
                      maand = as.integer(maand),
                      datum = as.Date(paste(jaar, maand, "01", sep = "-"))) |>
        dplyr::group_by(datum) |>
        dplyr::summarise(dplyr::across(dplyr::any_of(sel), ~ sum(.x, na.rm = TRUE)),
                         .groups = "drop") |>
        dplyr::rename_with(~ .BRMO_LABELS[.x], .cols = dplyr::any_of(names(.BRMO_LABELS))) |>
        (\(d) if (weergave() == "per100k")
          dplyr::mutate(d, dplyr::across(dplyr::where(is.numeric),
                                         ~ round(.x / inwoners_totaal * 100000, 2)))
         else d)() |>
        dplyr::arrange(datum)
    })
    
    # Micro-grafiek — gefilterd op geselecteerde pathogenen
    ggd_micro <- reactive({
      df <- data$regio()
      req(!is.null(df))
      sel <- brmo_sel()
      
      df |>
        dplyr::mutate(dplyr::across(dplyr::any_of(sel), as.numeric),
                      jaar = as.integer(jaar)) |>
        dplyr::group_by(jaar) |>
        dplyr::summarise(dplyr::across(dplyr::any_of(sel), ~ sum(.x, na.rm = TRUE)),
                         .groups = "drop") |>
        dplyr::rename_with(~ .BRMO_LABELS[.x], .cols = dplyr::any_of(names(.BRMO_LABELS))) |>
        tidyr::pivot_longer(cols = -jaar, names_to = "type", values_to = "waarde") |>
        dplyr::arrange(jaar)
    })
    
    # Respiratoir-reactives voor GGD
    resp_kolommen <- reactive({
      sel <- pathogenen()
      alle_v <- cfg$respiratoir$alle_virussen
      intersect(sel, alle_v)   # character(0) bij lege selectie → grafieken blijven leeg
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
      # Alle virussen laden zodat mod_kpi_server kan filteren op geselecteerde
      alle_v <- cfg$respiratoir$alle_virussen
      kpi_v  <- intersect(alle_v, names(df))
      inw    <- inwoners_totaal_r()
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
      sel <- resp_kolommen()
      req(length(sel) > 0)
      df |>
        dplyr::filter(nuts3 %in% noord_nuts3) |>
        dplyr::mutate(jaar = as.integer(jaar),
                      dplyr::across(dplyr::all_of(sel), as.numeric)) |>
        dplyr::group_by(jaar) |>
        dplyr::summarise(dplyr::across(dplyr::all_of(sel), ~ sum(.x, na.rm = TRUE)),
                         .groups = "drop") |>
        tidyr::pivot_longer(cols = dplyr::all_of(sel),
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
        list(trend = ggd_trend, micro = ggd_micro,
             regio = ggd_regio, shape = data$shape, kpi = ggd_kpi)
      }
    })
    
    mod_trend_server("trend",   ggd_data, cfg, eenheid = weergave, dataset = dataset)
    mod_kpi_server("kpi",       ggd_data, cfg, dataset = dataset, pathogenen = pathogenen)
    mod_micro_server("micro",   ggd_data, cfg, dataset = dataset, alle_geselecteerd = alle_geselecteerd)
    mod_regio_map_server("map", ggd_data, cfg, weergave, dataset = dataset)
  })
}