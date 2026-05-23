# =========================
# UI
# =========================
mod_regio_map_ui <- function(id) {
  ns <- NS(id)
  
  box(
    width = 5,
    class = "amr-regio-box",
    title = "BRMO incidentie per regio",
    
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
mod_regio_map_server <- function(id, data, cfg, weergave = reactive({ "per100k" })) {
  moduleServer(id, function(input, output, session) {
    
    # Noord-NL nuts3 regio's
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
      req(data$shape)
      req(data$regio())
      
      shp <- dplyr::filter(data$shape, nuts3 %in% noord_nuts3)
      dat <- data$regio()
      df  <- dplyr::left_join(shp, dat, by = c("nuts3" = "regio"))
      sf::st_transform(df, 4326)
    })
    
    output$map_plot <- renderLeaflet({
      df  <- kaart_df()
      pal <- leaflet::colorBin(
        palette  = cfg$colors$map_bins,
        domain   = df$incidentie,
        bins     = 4,
        na.color = "#E5E9F0"
      )
      bbox <- sf::st_bbox(df)
      
      leaflet::leaflet(df,
                       options = leaflet::leafletOptions(
                         zoomControl = FALSE, scrollWheelZoom = FALSE,
                         doubleClickZoom = FALSE, dragging = FALSE,
                         touchZoom = FALSE, attributionControl = FALSE
                       )
      ) |>
        leaflet::addPolygons(
          fillColor    = ~pal(incidentie),
          fillOpacity  = 0.9,
          color        = "white",
          weight       = 1.5,
          smoothFactor = 1,
          layerId      = ~nuts3,
          highlight    = leaflet::highlightOptions(
            weight = 2.5, color = "#1F3B63",
            fillOpacity = 1, bringToFront = TRUE
          ),
          label = ~paste0(nuts3, ": ", round(incidentie, 1)),
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
        ) |>
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
    
  })
}