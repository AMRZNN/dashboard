library(shiny)
library(leaflet)
library(dplyr)

mod_regio_map_ui <- function(id) {
  ns <- NS(id)
  
  box(
    width = 4,
    title = "BRMO incidentie per regio",
    tags$div(
      class="amr-subtitle",
      "Aantal meldingen per 100.000 inwoners, Noord-Nederland"
    ),
    leafletOutput(ns("map"), height="290px")
  )
}

mod_regio_map_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    
    output$map <- renderLeaflet({
      
      shp <- data$shape() %>%
        filter(name %in% cfg$geo$noord_provincies)
      
      dat <- data$regio() %>%
        filter(regio %in% cfg$geo$noord_provincies)
      
      df <- shp %>%
        left_join(dat, by = c("name"="regio"))
      
      pal <- colorBin(
        palette = cfg$colors$map_bins,
        domain  = df$incidentie,
        bins    = 4,
        na.color = "#E5E9F0"
      )
      
      leaflet(options = leafletOptions(
        zoomControl = FALSE,
        dragging = FALSE,
        scrollWheelZoom = FALSE
      )) %>%
        addPolygons(
          data = df,
          fillColor = ~pal(incidentie),
          weight = 1,
          color = "white",
          fillOpacity = 0.9,
          label = ~paste0(
            "<strong>", name, "</strong><br>",
            "Incidentie: ", round(incidentie,1),
            " per 100.000"
          ) %>% lapply(htmltools::HTML)
        ) %>%
        addLegend(
          pal = pal,
          values = df$incidentie,
          position = "bottomright",
          title = "Incidentie",
          opacity = 0.9
        ) %>%
        htmlwidgets::onRender("
          function(el,x){ el.style.background = 'white'; }
        ")
    })
  })
}