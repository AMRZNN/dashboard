
# =========================
# UI
# =========================
mod_trend_ui <- function(id) {
  ns <- NS(id)
  
  box(
    width = 7,
    class = "amr-fixed-box trend-box",
    title = "Incidentie van BRMO meldingen",
    
    tags$div(
      class = "amr-subtitle",
      "Aantal meldingen per 100.000 inwoners, Noord-Nederland, 2015–2024"
    ),
    
    tags$div(
      class = "trend-absolute-wrapper",
      
      tags$div(
        class = "trend-plot-area",
        girafeOutput(ns("plot"), width = "100%", height = "100%")
      ),
      
      tags$div(
        class = "trend-map-area",
        plotOutput(ns("mini_map"), height = "100%")
      )
    )
  )
}

# =========================
# SERVER
# =========================
mod_trend_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    
    # -------------------------
    # TREND DATA
    # -------------------------
    trend_df <- reactive({
      df <- data$trend()
      
      if ("regio" %in% names(df)) {
        df <- df %>% filter(regio == "Noord-Nederland")
      }
      
      df %>%
        arrange(jaar) %>%
        mutate(
          tooltip_txt = paste0(
            "Jaar: ", jaar,
            "\nIncidentie: ", round(incidentie, 1)
          ),
          jaar_chr = as.character(jaar)
        )
    })
    
    # -------------------------
    # TREND GRAFIEK (ggiraph + gradient)
    # -------------------------
    output$plot <- renderGirafe({
      df <- trend_df()
      req(nrow(df) > 0)
      
      label_df <- df %>%
        filter(jaar == max(jaar, na.rm = TRUE))
      
      # 🔥 Gradient instellingen
      n_layers <- 25
      
      gradient_layers <- lapply(seq_len(n_layers), function(i) {
        frac_upper <- i / n_layers
        frac_lower <- (i - 1) / n_layers
        
        geom_ribbon(
          data = df,
          aes(
            x = jaar,
            ymin = incidentie * frac_lower,
            ymax = incidentie * frac_upper
          ),
          fill = "#6EA6CF",
          alpha = 0.01 + 0.02 * i,   # 🔥 vloeiende opbouw
          inherit.aes = FALSE
        )
      })
      
      p <- ggplot(df, aes(x = jaar, y = incidentie)) +
        
        # 🔥 Gradient fill
        gradient_layers +
        
        # lijn
        geom_line(
          color = "#6EA6CF",
          linewidth = 1.4
        ) +
        
        # punten
        geom_point_interactive(
          aes(
            tooltip = tooltip_txt,
            data_id = jaar_chr
          ),
          size = 3,
          color = "#6EA6CF"
        ) +
        
        # label laatste punt
        geom_text(
          data = label_df,
          aes(
            label = format(round(incidentie, 1), decimal.mark = ",")
          ),
          nudge_y = 1.5,
          fontface = "bold",
          size = 5,
          color = "#1F3B63"
        ) +
        
        scale_x_continuous(breaks = df$jaar) +
        
        scale_y_continuous(
          expand = expansion(mult = c(0.05, 0.20))
        ) +
        
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
        width_svg = 7,
        height_svg = 2,
        options = list(
          opts_sizing(rescale = TRUE),
          opts_toolbar(saveaspng = FALSE)
        )
      )
    })
    
    # -------------------------
    # MINIMAP (HEEL NL)
    # -------------------------
    output$mini_map <- renderPlot({
      
      shp <- data$shape
      req(!is.null(shp))
      dat <- data$regio()
      req(!is.null(dat))
      
      df <- dplyr::left_join(shp, dat, by = c("provincie" = "regio"))
      
      ggplot(df) +
        geom_sf(
          aes(fill = incidentie),
          color = "white",
          linewidth = 0.3
        ) +
        coord_sf(expand = FALSE) +
        scale_fill_stepsn(
          colors = cfg$colors$map_bins,
          n.breaks = 4,
          na.value = "#E5E9F0"
        ) +
        theme_void() +
        theme(
          text = element_text(family = "Inter"),
          legend.position = "none",
          plot.margin = margin(0, 0, 0, 0)
        )
      
    }, bg = "transparent")
    
  })
}