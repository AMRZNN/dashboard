# =========================
# UI
# =========================
mod_regio_map_ui <- function(id) {
  ns <- NS(id)
  
  box(
    width = 5,
    class = "amr-regio-box",
    title = textOutput(ns("box_titel"), inline = TRUE),
    
    tags$div(
      style = "height: 280px; display:flex; flex-direction:column;",
      
      textOutput(ns("subtitel"), inline = FALSE) |> tagAppendAttributes(class = "amr-subtitle"),
      
      leafletOutput(ns("map_plot"), height = "100%", width = "100%")
    )
  )
}

# =========================
# SERVER
# geo_nuts3.rds heeft kolom: nuts3 (geen provincie)
# regio-data heeft kolom:    regio
# =========================
mod_regio_map_server <- function(id, data, cfg, weergave = reactive({ "per100k" }),
                                 dataset = reactive({ "brmo" }),
                                 regios  = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Geselecteerde regio's; standaard alle regio's
    if (is.null(regios)) regios <- reactive({ cfg$geo$noord_nuts3 })
    
    .d <- reactive({ if (is.reactive(data)) data() else data })
    
    output$box_titel <- renderText({
      ds <- if (is.reactive(dataset)) dataset() else "brmo"
      if (ds == "respiratoir") "Respiratoir incidentie per regio"
      else "BRMO incidentie per regio"
    })
    
    noord_nuts3 <- c(
      "Delfzijl en omgeving", "Oost-Groningen", "Overig Groningen",
      "Noord-Friesland", "Zuidoost-Friesland", "Zuidwest-Friesland",
      "Noord-Drenthe", "Zuidoost-Drenthe", "Zuidwest-Drenthe"
    )
    
    output$subtitel <- renderText({
      w <- if (is.function(weergave) || is.reactive(weergave)) weergave() else weergave
      if (w == "per100k") "Aantal meldingen per 100.000 inwoners" else "Absoluut aantal meldingen"
    })
    
    kaart_df <- reactive({
      d <- .d()
      req(d$shape)
      req(d$regio())
      
      shp <- dplyr::filter(d$shape, nuts3 %in% noord_nuts3)
      dat <- d$regio()
      df  <- dplyr::left_join(shp, dat, by = c("nuts3" = "regio"))
      sf::st_transform(df, 4326)
    })
    
    kaart_pal <- reactive({
      leaflet::colorBin(
        palette  = cfg$colors$map_bins,
        domain   = kaart_df()$incidentie,
        bins     = 4,
        na.color = "#E5E9F0"
      )
    })
    
    # Regio's tekenen; niet-geselecteerde regio's grijs
    teken_regios <- function(map, df, pal, sel) {
      df$geselecteerd <- df$nuts3 %in% sel
      leaflet::addPolygons(
        map, data = df,
        fillColor    = ~ifelse(geselecteerd, pal(incidentie), "#E5E9F0"),
        fillOpacity  = ~ifelse(geselecteerd, 0.9, 0.6),
        color        = "white",
        weight       = 1.5,
        smoothFactor = 1,
        layerId      = ~nuts3,
        highlight    = leaflet::highlightOptions(
          weight = 2.5, color = "#1F3B63",
          fillOpacity = 1, bringToFront = TRUE
        ),
        label = ~paste0(nuts3, ": ", round(incidentie, 1),
                        ifelse(geselecteerd, " (klik om te deselecteren)",
                               " (klik om te selecteren)")),
        labelOptions = leaflet::labelOptions(
          style     = list(
            "font-family"   = "Inter, sans-serif",
            "font-size"     = "13px",
            "background"    = "white",
            "border"        = "1px solid #E3E8EF",
            "border-radius" = "6px",
            "padding"       = "6px 10px",
            "box-shadow"    = "0 2px 6px rgba(0,0,0,0.15)"
          ),
          direction = "top",
          sticky    = TRUE,
          opacity   = 1
        )
      )
    }
    
    # Kaart opbouwen; de regioselectie wordt via de proxy hieronder bijgewerkt
    output$map_plot <- renderLeaflet({
      df   <- kaart_df()
      pal  <- kaart_pal()
      bbox <- sf::st_bbox(df)
      
      leaflet::leaflet(df,
                       options = leaflet::leafletOptions(
                         zoomControl = FALSE, scrollWheelZoom = FALSE,
                         doubleClickZoom = FALSE, dragging = FALSE,
                         touchZoom = FALSE, attributionControl = FALSE
                       )
      ) |>
        teken_regios(df, pal, isolate(regios())) |>
        leaflet::addLegend(
          position  = "bottomright",
          pal       = pal,
          values    = ~incidentie,
          title     = "Incidentie",
          opacity   = 0.9,
          labFormat = leaflet::labelFormat(digits = 1)
        ) |>
        leaflet::fitBounds(
          lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
        )
    })
    
    # Selectie gewijzigd → alleen de regio's opnieuw tekenen (geen volledige herbouw)
    observeEvent(regios(), ignoreInit = TRUE, {
      leaflet::leafletProxy("map_plot") |>
        leaflet::clearShapes() |>
        teken_regios(kaart_df(), kaart_pal(), regios())
    })
    
    # Klik op een regio → checkbox in de sidebar omzetten (via scripts.js)
    observeEvent(input$map_plot_shape_click, {
      regio <- input$map_plot_shape_click$id
      req(regio %in% cfg$geo$noord_nuts3)
      session$sendCustomMessage("toggle_regio", regio)
    })
    
  })
}