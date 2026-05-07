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

    tags$div(class = "amr-subtitle",
             "Aantal meldingen per 100.000 inwoners, Noord-Nederland, 2015–2024"),

    tags$div(
      class = "trend-inner",
      tags$div(class = "trend-plot-area",
               girafeOutput(ns("plot"), width = "100%", height = "100%")),
      tags$div(class = "trend-map-area",
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
      df |>
        dplyr::arrange(jaar) |>
        dplyr::mutate(
          tooltip_txt = paste0("Jaar: ", jaar, "\nIncidentie: ", round(incidentie, 1)),
          jaar_chr    = as.character(jaar)
        )
    })

    # -------------------------
    # TREND GRAFIEK
    # -------------------------
    output$plot <- renderGirafe({
      df <- trend_df()
      req(nrow(df) > 0)

      label_df <- dplyr::filter(df, jaar == max(jaar, na.rm = TRUE))

      n_layers <- 25
      gradient_layers <- lapply(seq_len(n_layers), function(i) {
        geom_ribbon(
          data = df,
          aes(x = jaar, ymin = incidentie * (i-1)/n_layers,
                         ymax = incidentie * i/n_layers),
          fill = "#6EA6CF", alpha = 0.01 + 0.02 * i, inherit.aes = FALSE
        )
      })

      p <- ggplot(df, aes(x = jaar, y = incidentie)) +
        gradient_layers +
        geom_line(color = "#6EA6CF", linewidth = 1.4) +
        geom_point_interactive(
          aes(tooltip = tooltip_txt, data_id = jaar_chr),
          size = 3, color = "#6EA6CF"
        ) +
        geom_text(
          data = label_df,
          aes(label = format(round(incidentie, 1), decimal.mark = ",")),
          nudge_y = 1.5, fontface = "bold", size = 5, color = "#1F3B63"
        ) +
        scale_x_continuous(breaks = df$jaar) +
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.20))) +
        theme_minimal() +
        theme(
          text = element_text(family = "Inter"),
          axis.title = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.margin = margin(10, 10, 10, 10)
        )

      girafe(
        ggobj = p,
        width_svg = 7, height_svg = 2,
        options = list(
          opts_sizing(rescale = TRUE),
          opts_toolbar(saveaspng = FALSE)
        )
      )
    })

    # -------------------------
    # MINIMAP via leaflet
    # -------------------------
    output$mini_map <- renderLeaflet({
      req(data$shape)
      req(data$regio())

      df <- dplyr::left_join(data$shape, data$regio(),
                             by = c("provincie" = "regio")) |>
            sf::st_transform(4326)

      pal  <- leaflet::colorBin(cfg$colors$map_bins, df$incidentie,
                                bins = 4, na.color = "#E5E9F0")
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
          fillColor   = ~pal(incidentie),
          fillOpacity = 0.9,
          color       = "white",
          weight      = 1,
          smoothFactor = 1
        ) |>
        leaflet::fitBounds(
          lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
        )
    })
  })
}
