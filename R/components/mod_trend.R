library(here)

# =========================
# UI
# =========================
mod_trend_ui <- function(id) {
  ns <- NS(id)
  
  box(
    width = 7,
    class = "amr-trend-box",
    title = "Incidentie van BRMO meldingen",
    
    textOutput(ns("subtitel"), inline = FALSE) |> tagAppendAttributes(class = "amr-subtitle"),
    
    tags$div(
      class = "trend-inner",
      tags$div(class = "trend-plot-area",
               plotlyOutput(ns("plot"), width = "100%", height = "100%")),
      tags$div(class = "trend-map-area",
               tags$div(class = "trend-map-year", textOutput(ns("map_year"))),
               leafletOutput(ns("mini_map"), width = "100%", height = "100%"))
    )
  )
}

# =========================
# SERVER
# =========================
mod_trend_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    
    trend_df <- reactive({
      df <- data$trend()
      if ("regio" %in% names(df)) {
        df <- dplyr::filter(df, regio == "Noord-Nederland")
      }
      # Ondersteuning voor zowel jaar- als maanddata
      if ("datum" %in% names(df)) {
        dplyr::arrange(df, datum)
      } else {
        dplyr::arrange(df, jaar)
      }
    })
    
    output$subtitel <- renderText({
      df <- trend_df()
      req(nrow(df) > 0)
      
      if ("datum" %in% names(df)) {
        eerste <- format(min(df$datum, na.rm = TRUE), "%b %Y")
        laatste <- format(max(df$datum, na.rm = TRUE), "%b %Y")
        paste0("Aantal meldingen, Noord-Nederland, ", eerste, " – ", laatste)
      } else {
        eerste <- min(df$jaar, na.rm = TRUE)
        laatste <- max(df$jaar, na.rm = TRUE)
        paste0("Aantal meldingen per 100.000 inwoners, Noord-Nederland, ", eerste, "\u2013", laatste)
      }
    })
    
    # -------------------------
    # TREND GRAFIEK via plotly
    # -------------------------
    output$plot <- renderPlotly({
      df <- trend_df()
      req(nrow(df) > 0)
      
      # Gebruik datum als x-as als die beschikbaar is, anders jaar
      gebruik_datum <- "datum" %in% names(df)
      
      if (gebruik_datum) {
        # Sorteer op datum en maak gesorteerde labels
        df <- dplyr::arrange(df, datum)
        df$x_as <- format(df$datum, "%b %Y")
      } else {
        df <- dplyr::arrange(df, jaar)
        df$x_as <- as.character(df$jaar)
      }
      
      x_label  <- if (gebruik_datum) "Maand" else "Jaar"
      last     <- df[nrow(df), ]
      # Bewaar volgorde voor plotly categoryorder
      x_volgorde <- df$x_as
      
      plotly::plot_ly(df, x = ~x_as, y = ~incidentie,
                      type = "scatter", mode = "lines+markers",
                      line  = list(color = "#6EA6CF", width = 2.5),
                      marker = list(color = "#6EA6CF", size = 7),
                      fill  = "tozeroy",
                      fillcolor = "rgba(110,166,207,0.15)",
                      hovertemplate = paste0(x_label, ": %{x}<br>Meldingen: %{y}<extra></extra>")) |>
        plotly::add_annotations(
          x = last$x_as, y = last$incidentie,
          text = as.character(last$incidentie),
          xanchor = "left", yanchor = "middle",
          showarrow = FALSE,
          font = list(size = 13, color = "#1F3B63", family = "Inter")
        ) |>
        plotly::layout(
          xaxis = list(
            tickfont = list(family = "Inter", size = 11, color = "#6B7C93"),
            showgrid = FALSE, zeroline = FALSE,
            title = "",
            type = if (gebruik_datum) "category" else "-",
            categoryorder = if (gebruik_datum) "array" else NULL,
            categoryarray = if (gebruik_datum) x_volgorde else NULL
          ),
          yaxis = list(
            tickfont = list(family = "Inter", size = 11, color = "#6B7C93"),
            showgrid = TRUE,
            gridcolor = "#E9EEF5",
            zeroline = FALSE,
            title = ""
          ),
          margin  = list(t = 5, r = 20, b = 30, l = 35),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          showlegend = FALSE,
          font = list(family = "Inter")
        ) |>
        plotly::config(displayModeBar = FALSE)
    })
    
    # -------------------------
    # JAARLABEL MINIMAP
    # -------------------------
    output$map_year <- renderText({
      df <- trend_df()
      req(nrow(df) > 0)
      
      if ("datum" %in% names(df)) {
        # Maanddata: toon "jan 2026" formaat
        format(max(df$datum, na.rm = TRUE), "%B %Y")
      } else {
        as.character(max(df$jaar, na.rm = TRUE))
      }
    })
    
    # -------------------------
    # MINIMAP via leaflet
    # -------------------------
    output$mini_map <- renderLeaflet({
      req(data$shape)
      
      # Gebruik regio_basis als die bestaat (Certe-modus), anders data$regio
      regio_data <- if (!is.null(data$regio_basis)) data$regio() else data$regio()
      
      df <- dplyr::left_join(data$shape, regio_data,
                             by = c("provincie" = "regio")) |>
        sf::st_transform(4326)
      
      pal  <- leaflet::colorBin(cfg$colors$map_bins, df$incidentie,
                                bins = 4, na.color = "#D0D5DC")
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
          weight       = 1,
          smoothFactor = 1
        ) |>
        leaflet::fitBounds(
          lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
        )
    })
  })
}