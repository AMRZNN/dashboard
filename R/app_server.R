app_server <- function(input, output, session, cfg) {
  
  data     <- data_service(cfg)
  weergave <- reactive({ if (is.null(input$weergave)) "absoluut" else input$weergave })
  dataset  <- reactive({ if (is.null(input$dataset))  "brmo"     else input$dataset  })
  
  # Geselecteerde pathogenen (kommagescheiden string → vector)
  # NULL = nog niet geladen (geeft fallback); "" = gebruiker heeft alles uitgevinkt (geeft leeg)
  pathogenen <- reactive({
    ds <- dataset()
    if (ds == "respiratoir") {
      raw <- input$pathogenen_resp
      if (is.null(raw)) return(c("Influenza A", "Influenza B", "RSV", "SARS-CoV-2"))
      if (raw == "") return(character(0))
      trimws(strsplit(raw, ",")[[1]])
    } else {
      raw <- input$pathogenen_brmo
      if (is.null(raw)) return(c("esbl", "mrsa", "vre", "cpe"))
      if (raw == "") return(character(0))
      trimws(strsplit(raw, ",")[[1]])
    }
  })
  
  # ------------------------------------------------------------------
  # Top-4 helpers: laatste beschikbare maand, gefilterd op Noord-NL
  # ------------------------------------------------------------------
  bereken_top4_brmo <- function() {
    # Gebruik regio-data: dit is de bron voor de micro-grafiek die de gebruiker ziet
    df <- tryCatch(data$regio(), error = function(e) NULL)
    # Fallback op certe als regio niet beschikbaar is
    if (is.null(df)) df <- tryCatch(data$certe(), error = function(e) NULL)
    if (is.null(df)) return(c("esbl","mrsa","vre","cpe"))
    .BRMO_COLS  <- c("esbl","mrsa","vre","cpe","mrpa","facre","cre","fara","cpa","ca")
    beschikbaar <- intersect(.BRMO_COLS, names(df))
    if (length(beschikbaar) == 0) return(c("esbl","mrsa","vre","cpe"))
    # Laatste beschikbare maand
    df <- df |>
      dplyr::mutate(jaar  = as.integer(jaar),
                    maand = as.integer(maand),
                    datum = as.Date(paste(jaar, maand, "01", sep = "-"))) |>
      dplyr::filter(datum == max(datum, na.rm = TRUE))
    totalen <- df |>
      dplyr::mutate(dplyr::across(dplyr::all_of(beschikbaar), as.numeric)) |>
      dplyr::summarise(dplyr::across(dplyr::all_of(beschikbaar), ~ sum(.x, na.rm = TRUE))) |>
      unlist()
    names(sort(totalen, decreasing = TRUE))[1:min(4, length(totalen))]
  }
  
  bereken_top4_resp <- function() {
    df <- tryCatch(data$respiratoir(), error = function(e) NULL)
    alle_v <- cfg$respiratoir$alle_virussen
    # Geef NULL terug als data niet beschikbaar — geen alfabetische fallback
    if (is.null(df)) return(NULL)
    noord_nuts3 <- c(
      "Delfzijl en omgeving", "Oost-Groningen", "Overig Groningen",
      "Noord-Friesland", "Zuidoost-Friesland", "Zuidwest-Friesland",
      "Noord-Drenthe", "Zuidoost-Drenthe", "Zuidwest-Drenthe"
    )
    beschikbaar <- intersect(alle_v, names(df))
    if (length(beschikbaar) == 0) return(NULL)
    # Filter op Noord-Nederland (zelfde als GGD-tab)
    if ("nuts3" %in% names(df))
      df <- dplyr::filter(df, nuts3 %in% noord_nuts3)
    df <- df |>
      dplyr::mutate(jaar  = as.integer(jaar),
                    maand = as.integer(maand),
                    datum = as.Date(paste(jaar, maand, "01", sep = "-"))) |>
      dplyr::filter(datum == max(datum, na.rm = TRUE))
    if (nrow(df) == 0) return(NULL)
    totalen <- df |>
      dplyr::mutate(dplyr::across(dplyr::all_of(beschikbaar), as.numeric)) |>
      dplyr::summarise(dplyr::across(dplyr::all_of(beschikbaar), ~ sum(.x, na.rm = TRUE))) |>
      unlist()
    names(sort(totalen, decreasing = TRUE))[1:min(4, length(totalen))]
  }
  
  # Stuur top-4 naar JS zodra data beschikbaar is — alleen als berekening lukt
  observe({
    top4 <- bereken_top4_brmo()
    req(!is.null(top4))
    session$sendCustomMessage("top4_brmo", as.list(top4))
  })
  
  observe({
    top4 <- bereken_top4_resp()
    req(!is.null(top4))
    session$sendCustomMessage("top4_resp", as.list(top4))
  })
  
  # JS vraagt on-demand (bij klik vóór data geladen)
  observeEvent(input$top4_request, {
    grp <- input$top4_request
    if (grp == "brmo") {
      top4 <- bereken_top4_brmo()
      if (!is.null(top4))
        session$sendCustomMessage("top4_brmo", as.list(top4))
    } else {
      top4 <- bereken_top4_resp()
      if (!is.null(top4))
        session$sendCustomMessage("top4_resp", as.list(top4))
    }
  })
  
  # Alle selectie-status: puur R — vergelijk aantal geselecteerden met volledige lijst
  .BRMO_ALLE <- c("esbl","mrsa","vre","cpe","mrpa","facre","cre","fara","cpa","ca")
  
  alle_geselecteerd <- reactive({
    ds <- dataset()
    sel <- pathogenen()
    if (ds == "respiratoir") {
      alle_v <- cfg$respiratoir$alle_virussen
      length(sel) >= length(alle_v) && length(setdiff(alle_v, sel)) == 0L
    } else {
      length(sel) >= length(.BRMO_ALLE) && length(setdiff(.BRMO_ALLE, sel)) == 0L
    }
  })
  
  mod_tab_ggd_server("ggd",           data, cfg, weergave, dataset, pathogenen, alle_geselecteerd)
  mod_tab_ziekenhuizen_server("zh",   data, cfg)
  mod_tab_laboratoria_server("lab",   data, cfg, weergave, dataset, pathogenen, alle_geselecteerd)
  mod_tab_huisartsen_server("ha",     data, cfg)
  mod_tab_verpleeghuizen_server("vh", data, cfg)
}