app_server <- function(input, output, session, cfg) {
  
  data     <- data_service(cfg)
  weergave <- reactive({ if (is.null(input$weergave)) "absoluut" else input$weergave })
  dataset  <- reactive({ if (is.null(input$dataset))  "brmo"     else input$dataset  })
  
  # Geselecteerde pathogenen (kommagescheiden string → vector)
  pathogenen <- reactive({
    ds <- dataset()
    if (ds == "respiratoir") {
      raw <- input$pathogenen_resp
      if (is.null(raw) || raw == "")
        return(c("Influenza A", "Influenza B", "RSV", "SARS-CoV-2"))
      trimws(strsplit(raw, ",")[[1]])
    } else {
      raw <- input$pathogenen_brmo
      if (is.null(raw) || raw == "")
        return(c("esbl", "mrsa", "vre", "cpe"))
      trimws(strsplit(raw, ",")[[1]])
    }
  })
  
  mod_tab_ggd_server("ggd",           data, cfg, weergave, dataset, pathogenen)
  mod_tab_ziekenhuizen_server("zh",   data, cfg)
  mod_tab_laboratoria_server("lab",   data, cfg, weergave, dataset, pathogenen)
  mod_tab_huisartsen_server("ha",     data, cfg)
  mod_tab_verpleeghuizen_server("vh", data, cfg)
}