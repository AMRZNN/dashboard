library(shiny)
library(ggplot2)
library(dplyr)
library(ggiraph)

# =========================
# UI
# =========================
mod_micro_ui <- function(id) {
  ns <- NS(id)
  
  box(
    width = 7,
    title = "Belangrijkste BRMO micro-organismen",
    
    tags$div(
      style = "padding: 0px; height: 450px; position: relative;",
      
      tags$div(
        class = "amr-subtitle",
        "Verdeeld naar micro-organismen, Noord-Nederland"
      ),
      
      girafeOutput(
        ns("plot"),
        width = "100%",
        height = "360px"   # 🔥 iets groter voor betere balans
      )
    )
  )
}

# =========================
# SERVER
# =========================
mod_micro_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    
    output$plot <- renderGirafe({
      
      df <- data$micro()
      
      req(nrow(df) > 0)
      
      p <- ggplot(df, aes(x = factor(jaar), y = waarde, fill = type)) +
        
        ggiraph::geom_col_interactive(
          aes(
            tooltip = paste0(type, ": ", round(waarde, 1))
          ),
          width = 0.8,
          color = "white"
        ) +
        
        scale_y_continuous(
          expand = expansion(mult = c(0, 0.1))
        ) +
        
        scale_x_discrete(
          expand = expansion(mult = c(0.05, 0.05))
        ) +
        
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
          
          # 🔥 GROTERE TEKST (FIX)
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.text = element_text(size = 14),
          
          # betere legenda spacing
          legend.key.size = unit(14, "pt"),
          
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          
          legend.position = "right",
          
          # iets meer ruimte rechts voor legenda
          plot.margin = margin(t = 5, r = 15, b = 5, l = 5)
        )
      
      girafe(
        ggobj = p,
        
        # 🔥 BELANGRIJK: kleinere SVG → betere schaal
        width_svg = 10,
        height_svg = 4,
        
        options = list(
          opts_sizing(rescale = TRUE),
          opts_toolbar(saveaspng = FALSE),
          opts_hover(css = "filter: brightness(1.1);")
        )
      )
    })
  })
}
