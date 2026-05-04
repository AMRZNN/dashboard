mod_regio_map_ui <- function(id) {
  ns <- NS(id)
  
  box(
    width = 5,
    title = "BRMO incidentie per regio",
    tags$div(
      style = "display: flex; flex-direction: column;",
      tags$div(
        class = "amr-subtitle",
        style = "width: 100%; text-align: left;",
        "Aantal meldingen per 100.000 inwoners"
      ),
      plotOutput(ns("map_plot"), height = "412px", width = "100%")
    )
  )
}

mod_regio_map_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    
    output$map_plot <- renderPlot({
      
      req(!is.null(data$shape))
      
      shp <- dplyr::filter(data$shape,
                           provincie %in% cfg$geo$noord_provincies)
      dat <- dplyr::filter(data$regio(),
                           regio %in% cfg$geo$noord_provincies)
      
      df <- dplyr::left_join(shp, dat, by = c("provincie" = "regio"))
      
      # Normaliseer geometrie naar 0-1 ruimte zodat coord_sf geen
      # vaste geografische aspect-ratio oplegt
      bbox  <- sf::st_bbox(df)
      x_range <- bbox["xmax"] - bbox["xmin"]
      y_range <- bbox["ymax"] - bbox["ymin"]
      df <- df |>
        dplyr::mutate(
          geometry = (geometry - c(bbox["xmin"], bbox["ymin"])) /
            max(x_range, y_range)
        )
      
      ggplot(data = df) +
        geom_sf(
          aes(fill = incidentie),
          color = "white",
          linewidth = 0.5,
          key_glyph = draw_key_dotplot
        ) +
        coord_sf(expand = FALSE) +
        scale_fill_stepsn(
          colors   = cfg$colors$map_bins,
          n.breaks = 4,
          na.value = "#E5E9F0",
          name     = "Incidentie",
          guide    = guide_legend(
            direction      = "horizontal",
            title.position = "top",
            override.aes   = list(
              shape  = 21,
              size   = 5,
              stroke = 0.5,
              color  = "white"
            )
          )
        ) +
        theme_void() +
        theme(
          text                  = element_text(family = "Inter"),
          legend.position       = "bottom",
          legend.justification  = "center",
          legend.direction      = "horizontal",
          legend.key            = element_blank(),
          legend.title          = element_text(
            size   = 11,
            face   = "bold",
            hjust  = 0.5,
            margin = margin(b = 6)
          ),
          legend.text           = element_text(size = 10),
          legend.spacing.x      = unit(8, "pt"),
          plot.margin           = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
          legend.background     = element_blank()
        )
      
    }, res = 96, bg = "transparent")
  })
}