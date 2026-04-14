library(ggplot2)
library(dplyr)
library(ggiraph)

mod_micro_ui <- function(id) {
  ns <- NS(id)
  
  box(
    width = 7,
    title = "Belangrijkste BRMO micro-organismen",
    # We halen alle witruimte uit de box
    tags$div(
      style = "padding: 0px; height: 450px; position: relative;", 
      tags$div(
        class = "amr-subtitle",
        "Verdeeld naar micro-organismen, Noord-Nederland"
      ),
      # Belangrijk: height="100%" en geen extra div eromheen
      ggiraph::girafeOutput(ns("plot"), width = "100%", height = "320px")
    )
  )
}


mod_micro_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    
    output$plot <- ggiraph::renderGirafe({
      
      df <- data$micro()
      
      p <- ggplot(df, aes(x = factor(jaar), y = waarde, fill = type)) +
        # GEEN size of linewidth parameters hier
        ggiraph::geom_col_interactive(
          aes(tooltip = paste0(type, ": ", round(waarde, 1))), 
          width = 0.8,
          color = "white"
        ) +
        scale_y_continuous(expand = c(0, 0.1)) +
        scale_x_discrete(expand = c(0.05, 0.05)) + 
        scale_fill_manual(
          values = c(
            "VRE"    = "#6EA6CF",
            "Overig" = "#95B9C7",
            "MRSA"   = "#ACCCBB",
            "ESBL"   = "#C2DEAF",
            "CPE"    = "#D1E6C9"
          ),
          name = NULL
        ) +
        theme_minimal() +
        theme(
          text = element_text(family = "Inter"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          legend.position = "right",
          # Minimale marges
          plot.margin = margin(t = 5, r = 0, b = 5, l = 0, unit = "pt")
        )
      
      girafe(
        ggobj = p,
        width_svg = 10,  # Breedte van het canvas
        height_svg = 4,  # Hoogte van het canvas
        options = list(
          # DIT IS DE FIX: Gebruik 'fill' om de container te dwingen
          opts_sizing(rescale = TRUE, width = 1),
          opts_toolbar(saveaspng = FALSE),
          opts_hover(css = "filter: brightness(1.1);")
        )
      )
    })
  })
}