library(here)

# Alle BRMO-kolommen
.BRMO_COLS <- c("esbl","mrsa","vre","cpe","mrpa","facre","cre","fara","cpa","ca")
.BRMO_LABELS <- c(esbl="ESBL", mrsa="MRSA", vre="VRE", cpe="CPE",
                  mrpa="MRPA", facre="FACRE", cre="CRE",
                  fara="FARA", cpa="CPA", ca="CA")

# =========================
# UI
# =========================
mod_tab_laboratoria_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class = "amr-row1",
             mod_trend_ui(ns("trend")),
             mod_kpi_ui(ns("kpi"))
    ),
    fluidRow(class = "amr-row2",
             mod_micro_ui(ns("micro")),
             mod_regio_map_ui(ns("map"))
    )
  )
}

# =========================
# SERVER
# =========================
mod_tab_laboratoria_server <- function(id, data, cfg,
                                       weergave   = reactive({ "absoluut" }),
                                       dataset    = reactive({ "brmo" }),
                                       pathogenen = reactive({ c("esbl","mrsa","vre","cpe") })) {
  moduleServer(id, function(input, output, session) {
    
    noord      <- cfg$geo$noord_provincies
    noord_nuts3 <- c("Delfzijl en omgeving","Oost-Groningen","Overig Groningen",
                     "Noord-Friesland","Zuidoost-Friesland","Zuidwest-Friesland",
                     "Noord-Drenthe","Zuidoost-Drenthe","Zuidwest-Drenthe")
    
    inwoners_noord <- reactive({
      sum(sf::st_drop_geometry(data$shape) |>
            dplyr::filter(nuts3 %in% noord_nuts3) |>
            dplyr::pull(inwoners), na.rm = TRUE)
    })
    
    # Geselecteerde BRMO-kolommen (veilig: alleen bestaande kolommen)
    brmo_sel <- reactive({
      sel <- pathogenen()
      intersect(sel, .BRMO_COLS)
    })
    
    # -------------------------------------------------------
    # BRMO-reactives — gefilterd op geselecteerde pathogenen
    # -------------------------------------------------------
    brmo_base <- reactive({
      df <- data$certe()
      req(!is.null(df))
      df |>
        dplyr::filter(provincie %in% noord) |>
        dplyr::mutate(dplyr::across(dplyr::all_of(.BRMO_COLS), as.numeric),
                      jaar  = as.integer(jaar),
                      maand = as.integer(maand),
                      datum = as.Date(paste(jaar, maand, "01", sep = "-")))
    })
    
    brmo_trend <- reactive({
      df  <- brmo_base()
      sel <- brmo_sel()
      req(length(sel) > 0)
      df |>
        dplyr::mutate(totaal = rowSums(dplyr::across(dplyr::all_of(sel)), na.rm = TRUE)) |>
        dplyr::group_by(datum, jaar, maand) |>
        dplyr::summarise(meldingen = sum(totaal, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(datum) |>
        (\(d) { cutoff <- seq(max(d$datum), length.out = 2, by = "-11 months")[2]
        dplyr::filter(d, datum >= cutoff) })() |>
        dplyr::mutate(incidentie = if (weergave() == "per100k")
          round(meldingen / inwoners_noord() * 100000, 1) else meldingen)
    })
    
    # KPI: altijd alle kolommen laden → top-4 op basis van laatste maand
    brmo_kpi <- reactive({
      df <- brmo_base()
      inw <- inwoners_noord()
      result <- df |>
        dplyr::group_by(datum) |>
        dplyr::summarise(dplyr::across(dplyr::all_of(.BRMO_COLS),
                                       ~ sum(.x, na.rm = TRUE)), .groups = "drop") |>
        dplyr::arrange(datum)
      if (weergave() == "per100k") {
        result <- result |>
          dplyr::mutate(dplyr::across(dplyr::all_of(.BRMO_COLS),
                                      ~ round(.x / inw * 100000, 2)))
      }
      # Hernoem kolommen: esbl→ESBL etc.
      # dplyr::rename verwacht c(nieuw = "oud"), dus draaien we .BRMO_LABELS om
      result |> dplyr::rename_with(
        ~ .BRMO_LABELS[.x],
        .cols = dplyr::any_of(names(.BRMO_LABELS))
      )
    })
    
    brmo_micro <- reactive({
      df  <- brmo_base()
      sel <- brmo_sel()
      req(length(sel) > 0)
      df |>
        dplyr::group_by(jaar) |>
        dplyr::summarise(dplyr::across(dplyr::all_of(sel), ~ sum(.x, na.rm = TRUE)),
                         .groups = "drop") |>
        dplyr::rename_with(~ .BRMO_LABELS[.x], .cols = dplyr::any_of(names(.BRMO_LABELS))) |>
        tidyr::pivot_longer(cols = -jaar, names_to = "type", values_to = "waarde") |>
        dplyr::arrange(jaar)
    })
    
    brmo_regio <- reactive({
      df  <- brmo_base()
      sel <- brmo_sel()
      req(length(sel) > 0)
      df |>
        dplyr::mutate(totaal = rowSums(dplyr::across(dplyr::all_of(sel)), na.rm = TRUE)) |>
        dplyr::filter(datum == max(datum, na.rm = TRUE)) |>
        dplyr::group_by(regio = nuts3) |>
        dplyr::summarise(meldingen = sum(totaal, na.rm = TRUE), .groups = "drop") |>
        dplyr::left_join(sf::st_drop_geometry(data$shape) |> dplyr::select(nuts3, inwoners),
                         by = c("regio" = "nuts3")) |>
        dplyr::mutate(incidentie = if (weergave() == "per100k")
          round(meldingen / inwoners * 100000, 1) else meldingen)
    })
    
    # -------------------------------------------------------
    # RESPIRATOIR-reactives — gefilterd op geselecteerde virussen
    # -------------------------------------------------------
    alle_resp <- cfg$respiratoir$alle_virussen
    
    resp_sel <- reactive({
      sel <- pathogenen()
      intersect(sel, alle_resp)
    })
    
    resp_base <- reactive({
      df <- data$respiratoir()
      req(!is.null(df))
      df |>
        dplyr::filter(provincie %in% noord) |>
        dplyr::mutate(dplyr::across(dplyr::all_of(alle_resp), as.numeric),
                      jaar  = as.integer(jaar),
                      maand = as.integer(maand),
                      datum = as.Date(paste(jaar, maand, "01", sep = "-")))
    })
    
    resp_trend <- reactive({
      df  <- resp_base()
      sel <- resp_sel()
      req(length(sel) > 0)
      df |>
        dplyr::mutate(totaal = rowSums(dplyr::across(dplyr::all_of(sel)), na.rm = TRUE)) |>
        dplyr::group_by(datum, jaar, maand) |>
        dplyr::summarise(meldingen = sum(totaal, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(datum) |>
        (\(d) { cutoff <- seq(max(d$datum), length.out = 2, by = "-11 months")[2]
        dplyr::filter(d, datum >= cutoff) })() |>
        dplyr::mutate(incidentie = if (weergave() == "per100k")
          round(meldingen / inwoners_noord() * 100000, 1) else meldingen)
    })
    
    # KPI: altijd alle virussen laden → top-4 op laatste maand
    resp_kpi <- reactive({
      df  <- resp_base()
      inw <- inwoners_noord()
      result <- df |>
        dplyr::group_by(datum) |>
        dplyr::summarise(dplyr::across(dplyr::all_of(alle_resp),
                                       ~ sum(.x, na.rm = TRUE)), .groups = "drop") |>
        dplyr::arrange(datum)
      if (weergave() == "per100k") {
        result <- result |>
          dplyr::mutate(dplyr::across(dplyr::all_of(alle_resp),
                                      ~ round(.x / inw * 100000, 2)))
      }
      result
    })
    
    resp_micro <- reactive({
      df  <- resp_base()
      sel <- resp_sel()
      req(length(sel) > 0)
      df |>
        dplyr::group_by(jaar) |>
        dplyr::summarise(dplyr::across(dplyr::all_of(sel), ~ sum(.x, na.rm = TRUE)),
                         .groups = "drop") |>
        tidyr::pivot_longer(cols = dplyr::all_of(sel),
                            names_to = "type", values_to = "waarde") |>
        dplyr::arrange(jaar)
    })
    
    resp_regio <- reactive({
      df  <- resp_base()
      sel <- resp_sel()
      req(length(sel) > 0)
      df |>
        dplyr::mutate(totaal = rowSums(dplyr::across(dplyr::all_of(sel)), na.rm = TRUE)) |>
        dplyr::filter(datum == max(datum, na.rm = TRUE)) |>
        dplyr::group_by(regio = nuts3) |>
        dplyr::summarise(meldingen = sum(totaal, na.rm = TRUE), .groups = "drop") |>
        dplyr::left_join(sf::st_drop_geometry(data$shape) |> dplyr::select(nuts3, inwoners),
                         by = c("regio" = "nuts3")) |>
        dplyr::mutate(incidentie = if (weergave() == "per100k")
          round(meldingen / inwoners * 100000, 1) else meldingen)
    })
    
    # -------------------------------------------------------
    # Actief data-object
    # -------------------------------------------------------
    lab_data <- reactive({
      if (dataset() == "respiratoir") {
        list(trend = resp_trend, micro = resp_micro,
             regio = resp_regio, shape = data$shape, kpi = resp_kpi)
      } else {
        list(trend = brmo_trend, micro = brmo_micro,
             regio = brmo_regio, shape = data$shape, kpi = brmo_kpi)
      }
    })
    
    mod_trend_server("trend",   lab_data, cfg, eenheid = weergave, dataset = dataset)
    mod_kpi_server(  "kpi",     lab_data, cfg, dataset = dataset)
    mod_micro_server("micro",   lab_data, cfg, dataset = dataset)
    mod_regio_map_server("map", lab_data, cfg, weergave, dataset = dataset)
  })
}