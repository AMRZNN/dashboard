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
      
      tags$div(class = "amr-subtitle",
               "Aantal meldingen per 100.000 inwoners"),
      
      leafletOutput(ns("map_plot"), height = "100%", width = "100%")
    )
  )
}

# =========================
# SERVER
# geo_nuts3.rds heeft kolom: nuts3 (geen provincie)
# regio-data heeft kolom:    regio
# =========================
mod_regio_map_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    
    # Noord-NL nuts3 regio's
    noord_nuts3 <- c(
      "Delfzijl en omgeving", "Oost-Groningen", "Overig Groningen",
      "Noord-Friesland", "Zuidoost-Friesland", "Zuidwest-Friesland",
      "Noord-Drenthe", "Zuidoost-Drenthe", "Zuidwest-Drenthe"
    )
    
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
    
    # Popup bij klik
    observeEvent(input$map_plot_shape_click, {
      click <- input$map_plot_shape_click
      req(click)
      
      df  <- kaart_df()
      rij <- df[df$nuts3 == click$id, ]
      req(nrow(rij) > 0)
      
      centroid <- sf::st_centroid(rij$geometry)
      coords   <- sf::st_coordinates(centroid)
      bbox     <- sf::st_bbox(df)
      
      leaflet::leafletProxy("map_plot", session) |>
        leaflet::clearPopups() |>
        leaflet::fitBounds(
          lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
        ) |>
        leaflet::addPopups(
          lng   = coords[1, "X"],
          lat   = coords[1, "Y"],
          popup = sprintf(
            "<div style='font-family:Inter,sans-serif;font-size:14px;padding:4px 2px;'>
              <strong style='font-size:15px;'>%s</strong><br/>
              <span style='color:#6B7C93;'>Incidentie:</span>
              <strong>%.1f</strong>
            </div>",
            rij$nuts3, rij$incidentie
          ),
          options = leaflet::popupOptions(closeButton = TRUE, maxWidth = 220, minWidth = 160)
        )
    })
  })
}