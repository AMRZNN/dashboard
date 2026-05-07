
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
# =========================
mod_regio_map_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    
    output$map_plot <- renderLeaflet({
      
      req(data$shape)
      req(data$regio())
      
      shp <- dplyr::filter(data$shape,
                           provincie %in% cfg$geo$noord_provincies)
      dat <- dplyr::filter(data$regio(),
                           regio %in% cfg$geo$noord_provincies)
      
      df <- dplyr::left_join(shp, dat, by = c("provincie" = "regio"))
      df <- sf::st_transform(df, 4326)
      
      pal <- leaflet::colorBin(
        palette  = cfg$colors$map_bins,
        domain   = df$incidentie,
        bins     = 4,
        na.color = "#E5E9F0"
      )
      
      bbox <- sf::st_bbox(df)
      
      leaflet::leaflet(df,
                       options = leaflet::leafletOptions(
                         zoomControl        = FALSE,
                         scrollWheelZoom    = FALSE,
                         doubleClickZoom    = FALSE,
                         dragging           = FALSE,
                         touchZoom          = FALSE,
                         attributionControl = FALSE
                       )
      ) |>
        leaflet::addPolygons(
          fillColor    = ~pal(incidentie),
          fillOpacity  = 0.9,
          color        = "white",
          weight       = 1.5,
          smoothFactor = 1,
          layerId      = ~provincie,
          highlight    = leaflet::highlightOptions(
            weight       = 2.5,
            color        = "#1F3B63",
            fillOpacity  = 1,
            bringToFront = TRUE
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
    
    # Popup via Shiny click-event ipv leaflet popup
    observeEvent(input$map_plot_shape_click, {
      click <- input$map_plot_shape_click
      req(click)
      
      shp <- dplyr::filter(data$shape,
                           provincie %in% cfg$geo$noord_provincies)
      dat <- dplyr::filter(data$regio(),
                           regio %in% cfg$geo$noord_provincies)
      df  <- dplyr::left_join(shp, dat, by = c("provincie" = "regio"))
      df  <- sf::st_transform(df, 4326)
      
      rij <- df[df$provincie == click$id, ]
      
      # Centroïde van de provincie als ankerpunt voor de popup
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
          lng     = coords[1, "X"],
          lat     = coords[1, "Y"],
          popup   = sprintf(
            "<div style='font-family:Inter,sans-serif;font-size:14px;padding:4px 2px;'>
              <strong style='font-size:15px;'>%s</strong><br/>
              <span style='color:#6B7C93;'>Incidentie:</span>
              <strong>%.1f</strong>
            </div>",
            rij$provincie, rij$incidentie
          ),
          options = leaflet::popupOptions(
            closeButton = TRUE,
            maxWidth    = 220,
            minWidth    = 160
          )
        )
    })
  })
}
